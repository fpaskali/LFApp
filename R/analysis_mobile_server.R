analysis_mobile_server <- function(input, output, session){
  ########## FIRST TAB
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  oldopt <- options()
  on.exit(options(oldopt))
  options(shiny.maxRequestSize=100*1024^2) #file can be up to 50 mb; default is 5 mb
  ## initializations
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL, shiny_img_cropped = NULL,
                                   shiny_img_final = NULL, Threshold = NULL)
  IntensData <- NULL
  ExpInfo <- NULL
  MergedData <- NULL
  FILENAME <- NULL
  fit <- NULL
  modelPlot <- NULL
  LOB <- NULL
  LOD <- NULL
  LOQ <- NULL
  calFun <- NULL
  quanData <- NULL
  CalibrationData <- NULL
  predFunc <- NULL
  predictData <- NULL

  # checks upload for file imput
  observe({
    #default: upload image
    if(input$upload == "Upload image"){
      output$plot1 <- renderPlot({
        if(is.null(input$file1))
          return(NULL)
        validate(need(!is.null(input$file1), "Must upload a valid jpg, png, or tiff"))
      })
    }
    if(input$upload == "Sample"){
      # using sample image
      img <- readImage(system.file("images", "sample.TIF", package="LFApp"))
      shinyImageFile$shiny_img_origin <- img
      shinyImageFile$shiny_img_cropped <- img
      shinyImageFile$shiny_img_final <- img

      shinyImageFile$filename <- "sample.TIF"
      #outputs image to plot1 -- main plot
      output$plot1 <- renderPlot({ EBImage::display(shinyImageFile$shiny_img_final, method = "raster") })
    }
    drawCropButtons()
  })

  # if the new file is entered, it will become new ImageFile
  observeEvent(input$file1, {
    shinyImageFile$filename <- input$file1$name
    img <- readImage(input$file1$datapath)
    shinyImageFile$shiny_img_origin <- img
    shinyImageFile$shiny_img_cropped <- img
    shinyImageFile$shiny_img_final <- img
    output$plot1 <- renderPlot({EBImage::display(img, method = "raster")})
  })

  drawCropButtons <- function(resetButton=FALSE, segButton=FALSE) {
    output$cropButtons <- renderUI({
      tagList(
        f7Segment(
          if (resetButton) {
            f7Button("reset", color = "blue", label = "Reset")
          } else {
            f7Button("no-reset", color = "gray", label = "Reset")
          },
          if (segButton) {
            f7Button("segmentation", color="green", label = "Apply Segmentation")
          } else {
            f7Button("no-segmentation", color="gray", label = "Apply Segmentation")
          }
        )
      )
    })
  }

  # Rotation ----------------------------------------------------------------

  observe({reactiveRotation()})

  reactiveRotation <- eventReactive(input$rotate, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_final <- EBImage::rotate(shinyImageFile$shiny_img_cropped, input$rotate,
                                                          bg.col="white")
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })

  observe({reactiveRotationCCW()})

  reactiveRotationCCW <- eventReactive(input$rotateCCW, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::rotate(shinyImageFile$shiny_img_cropped, -90)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })

  observe({reactiveRotationCW()})

  reactiveRotationCW <- eventReactive(input$rotateCW, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::rotate(shinyImageFile$shiny_img_cropped, 90)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })

  observe({reactiveRotationFlip()})

  reactiveRotationFlip <- eventReactive(input$fliphor, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::flip(shinyImageFile$shiny_img_cropped)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })

  observe({reactiveRotationFlop()})

  reactiveRotationFlop <- eventReactive(input$flipver, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::flop(shinyImageFile$shiny_img_cropped)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })

  croppedImage <- function(image, xmin, ymin, xmax, ymax){
    if(length(dim(image)) == 2)
      image <- image[xmin:xmax, ymin:ymax, drop = FALSE]
    else if(length(dim(image)) == 3)
      image <- image[xmin:xmax, ymin:ymax, ,drop = FALSE]
    return(image)
  }

  observe({resetImage()})

  resetImage <- eventReactive(input$reset,{
    isolate({
      shinyImageFile$shiny_img_cropped <- shinyImageFile$shiny_img_origin
      shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
      output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
      session$resetBrush("plot_brush")
      updateSliderInput(session, "rotate", value=0)
      drawCropButtons()
    })
  })


  #prompts shiny to look at recursive crop
  observe({recursiveCrop()})

  #only executes when selection box is double clicked
  recursiveCrop <- eventReactive(input$plot_dblclick,{
    isolate({
      p <- input$plot_brush
      validate(need(p$xmax <= dim(shinyImageFile$shiny_img_cropped)[1],
                    "Highlighted portion is out of bounds on the x-axis"))
      validate(need(p$ymax <= dim(shinyImageFile$shiny_img_cropped)[2],
                    "Highlighted portion is out of bounds on the y-axis"))
      validate(need(p$xmin >= 0,
                    "Highlighted portion is out of bounds on the x-axis"))
      validate(need(p$ymin >= 0,
                    "Highlighted portion is out of bounds on the y-axis"))
      shinyImageFile$shiny_img_cropped <- croppedImage(shinyImageFile$shiny_img_final, p$xmin, p$ymin, p$xmax, p$ymax)
      shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
      output$plot1 <- renderPlot({
        EBImage::display(shinyImageFile$shiny_img_final, method = "raster")
      })
      updateF7Slider("rotate", value=0)
      session$resetBrush("plot_brush")
      drawCropButtons(resetButton = TRUE)
    })
    session$resetBrush("plot_brush")
  })


  observe({recursiveGrid()})

  recursiveGrid <- eventReactive(input$plot_brush,{
    isolate({
      p <- input$plot_brush
      output$plot1 <- renderPlot({
        EBImage::display(shinyImageFile$shiny_img_final, method = "raster")

        colcuts <- seq(p$xmin, p$xmax, length.out = input$strips + 1)
        rowcuts <- seq(p$ymin, p$ymax, length.out = 2*input$bands) # bands + spaces between bands

        for (x in colcuts) {
          lines(x = rep(x, 2), y = c(p$ymin, p$ymax), col="red")
        }
        for (y in rowcuts) {
          lines(x = c(p$xmin, p$xmax), y = rep(y, 2), col="red")
        }
      })
      drawCropButtons(segButton = TRUE, resetButton = TRUE)
    })
  })


  observe({recursiveSegmentation()})

  recursiveSegmentation <- eventReactive(input$segmentation,{
    isolate({
      p <- input$plot_brush
      # Check if the region of interest is out of the bounds
      if (p$xmax <= dim(shinyImageFile$shiny_img_cropped)[1] &&
          p$ymax <= dim(shinyImageFile$shiny_img_cropped)[2] &&
          p$xmin >= 0 &&
          p$ymin >= 0) {
        MAX <- dim(shinyImageFile$shiny_img_cropped)[1:2]
        colcuts <- seq(p$xmin, p$xmax, length.out = input$strips + 1)
        rowcuts <- seq(p$ymin, p$ymax, length.out = 2*input$bands)

        segmentation.list <- vector("list", length = input$strips)
        count <- 0
        for(i in 1:input$strips){
          tmp.list <- vector("list", length = 2*input$bands-1)
          for(j in 1:(2*input$bands-1)){
            img <- shinyImageFile$shiny_img_final
            if(length(dim(img)) == 2)
              img <- img[colcuts[i]:colcuts[i+1], rowcuts[j]:rowcuts[j+1]]
            else if(length(dim(img)) == 3)
              img <- img[colcuts[i]:colcuts[i+1], rowcuts[j]:rowcuts[j+1], , drop = FALSE]
            tmp.list[[j]] <- img
          }
          segmentation.list[[i]] <- tmp.list
        }
        shinyImageFile$cropping_grid <- list("columns" = colcuts, "rows" = rowcuts)
        shinyImageFile$segmentation_list <- segmentation.list
        updateF7Tabs(session=session, id="tabs", selected = "Background")
      } else {
        f7Toast(text="Error: The grid is out of bounds", position="bottom", session=session)
      }
    })
  })

  ################ END OF FIRST TAB

  ############## SECOND TAB
  observe({
    input$thresh
    updateF7Stepper("selectStrip", max=input$strips)
  })

  showThresholdPlots <- function() {
    output$threshPlots <- renderUI({
      tagList(
        f7Block(
          hairlines = FALSE,
          strong = TRUE,
          inset = FALSE,
          h3('Background Correction', align = "center"),
          verbatimTextOutput("thresh"),
          h4('Signal Intensity Above Background', align = "center"),
          plotOutput("plot3"),
          h4('Lines After Background Subtraction', align = "center"),
          plotOutput("plot4"),
          verbatimTextOutput("meanIntens"),
          verbatimTextOutput("medianIntens"),
          f7Segment(
            f7Button("data", color="green", label = "Add to Data"),
            f7Button("showIntensData", label = "Switch To Intensity Data")
          )
        )
      )
    })
  }

  observe({recursiveThreshold()})

  recursiveThreshold <- eventReactive(input$threshold,{
    isolate({
      showThresholdPlots()
      seg.list <- shinyImageFile$segmentation_list
      i <- input$selectStrip
      if(input$thresh == "Quantile"){
        Background <- vector(mode = "list", length = input$bands)
        for(j in 1:input$bands){
          img <- seg.list[[i]][[j]]
          if(colorMode(img) > 0){
            img <- 1-channel(img, input$channel)
          }
          if(input$invert=="Yes") {
            img <- 1 - img
          }
          Background[[j]] <- as.numeric(EBImage::imageData(img))
        }
        Background.Threshold <- quantile(unlist(Background),
                                         probs = input$quantile1/100)
        shinyImageFile$Threshold <- Background.Threshold
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count <- 0
          for(j in Bands){
            count <- count + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1 - img
            }
            signal <- EBImage::imageData(img) > Background.Threshold
            EBImage::imageData(img) <- signal
            plot(img)
            title(paste0("Line ", count))
          }
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count <- 0
          for(j in Bands){
            count <- count + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1 - img
            }
            signal <- EBImage::imageData(img) > Background.Threshold
            EBImage::imageData(img) <- (EBImage::imageData(img) - Background.Threshold)*signal
            shinyImageFile$Mean_Intensities[1,count] <- mean(EBImage::imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count] <- median(EBImage::imageData(img)[signal])
            plot(img)
            title(paste0("Line ", count))
          }
        })
      }
      else if(input$thresh == "Otsu"){
        Background.Threshold <- numeric(input$bands)
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1 - img
            }
            Background.Threshold[count1] <- otsu(img)
            signal <- EBImage::imageData(img) > Background.Threshold[count1]
            EBImage::imageData(img) <- signal
            plot(img)
            title(paste0("Line ", count2))
          }
          shinyImageFile$Threshold <- Background.Threshold
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1 - img
            }
            thr <- otsu(img)
            signal <- EBImage::imageData(img) > thr
            EBImage::imageData(img) <- (EBImage::imageData(img) - thr)*signal
            shinyImageFile$Mean_Intensities[1,count1] <- mean(EBImage::imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count1] <- median(EBImage::imageData(img)[signal])
            plot(img)
            title(paste0("Line ", count2))
          }
        })
      }
      else if(input$thresh == "Triangle"){
        Background.Threshold <- numeric(input$bands)
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1 - img
            }
            Background.Threshold[count1] <- triangle(img, input$tri_offset)
            signal <- EBImage::imageData(img) > Background.Threshold[count1]
            EBImage::imageData(img) <- signal
            plot(img)
            title(paste0("Line ", count2))
          }
          shinyImageFile$Threshold <- Background.Threshold
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1 - img
            }
            thr <- triangle(img, input$tri_offset)
            signal <- EBImage::imageData(img) > thr
            EBImage::imageData(img) <- (EBImage::imageData(img) - thr)*signal
            shinyImageFile$Mean_Intensities[1,count1] <- mean(EBImage::imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count1] <- median(EBImage::imageData(img)[signal])
            plot(img)
            title(paste0("Line ", count2))
          }
        })
      }
      else if(input$thresh == "Li"){
        Background.Threshold <- numeric(input$bands)
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1-img
            }
            Background.Threshold[count1] <- threshold_li(img)
            signal <- EBImage::imageData(img) > Background.Threshold[count1]
            EBImage::imageData(img) <- signal
            plot(img)
            title(paste0("Line ", count2))
          }
          shinyImageFile$Threshold <- Background.Threshold
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if(input$invert=="Yes") {
              img <- 1-img
            }
            thr <- threshold_li(img)
            signal <- EBImage::imageData(img) > thr
            EBImage::imageData(img) <- (EBImage::imageData(img) - thr)*signal
            shinyImageFile$Mean_Intensities[1,count1] <- mean(EBImage::imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count1] <- median(EBImage::imageData(img)[signal])
            plot(img)
            title(paste0("Line ", count2))
          }
        })
      }
    })
  })

  output$thresh <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Threshold(s): ", paste0(signif(shinyImageFile$Threshold, 4), collapse = ", "))
  })
  output$meanIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Mean intensities: ", paste0(signif(shinyImageFile$Mean_Intensities, 4), collapse = ", "))
  })
  output$medianIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Median intensities: ", paste0(signif(shinyImageFile$Median_Intensities, 4), collapse = ", "))
  })

  observe({recursiveData()})

  recursiveData <- eventReactive(input$data,{
    isolate({
      AM <- shinyImageFile$Mean_Intensities
      colnames(AM) <- paste0("Mean", 1:input$bands)
      Med <- shinyImageFile$Median_Intensities
      colnames(Med) <- paste0("Median", 1:input$bands)
      if(input$thresh == "Otsu"){
        BG.method <- matrix(c("Otsu", NA, NA), nrow = 1,
                            ncol = 3, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Offset", "Probability")
      }
      if(input$thresh == "Quantile"){
        BG.method <- matrix(c("quantile", NA, input$quantile1),
                            nrow = 1, ncol = 3, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Offset", "Probability")
      }
      if(input$thresh == "Triangle"){
        BG.method <- matrix(c("triangle", input$tri_offset, NA), nrow = 1,
                            ncol = 3, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Offset", "Probability")
      }
      if(input$thresh == "Li"){
        BG.method <- matrix(c("Li", NA, NA), nrow = 1,
                            ncol = 3, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Offset", "Probability")
      }
      seg.list <- shinyImageFile$segmentation_list
      img <- seg.list[[1]][[1]]
      if(colorMode(img) > 0){
        MODE <- input$channel
        DF <- data.frame("File" = shinyImageFile$filename,
                         "Mode" = MODE,
                         "Strip" = input$selectStrip,
                         BG.method, AM, Med,
                         check.names = FALSE)
      }else{
        DF <- data.frame("File" = shinyImageFile$filename,
                         "Mode" = NA,
                         "Strip" = input$selectStrip,
                         BG.method, AM, Med,
                         check.names = FALSE)
      }
      if(inherits(try(IntensData, silent = TRUE), "try-error"))
        IntensData <<- DF
      else
        IntensData <<- rbind(IntensData, DF)

      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
      output$plot3 <- NULL
      output$plot4 <- NULL
      output$threshPlots <- NULL
      if(!is.null(shinyImageFile$Threshold))
        shinyImageFile$Threshold <- NULL
      if(!is.null(shinyImageFile$Mean_Intensities))
        shinyImageFile$Mean_Intensities <- NULL
      if(!is.null(shinyImageFile$Median_Intensities))
        shinyImageFile$Median_Intensities <- NULL
    })
  })

  output$intens <- renderDT({
    DF <- IntensData
    datatable(DF)
  })

  observe({recursiveShowIntensData()})
  recursiveShowIntensData <- eventReactive(input$showIntensData,{
    isolate({
      updateF7Tabs(session=session, id="tabs", selected = "IntensityData")
    })
  })

  observe({recursiveDelete()})
  recursiveDelete <- eventReactive(input$deleteData,{
    isolate({
      IntensData <<- NULL
      output$intens <- renderDT({})
    })
  })

  observe({recursiveDelete2()})
  recursiveDelete2 <- eventReactive(input$deleteData2,{
    isolate({
      ExpInfo <<- NULL
      MergedData <<- NULL
      output$experiment <- renderDT({})
    })
  })

  observe({recursiveDelete3()})
  recursiveDelete3 <- eventReactive(input$deleteData3,{
    isolate({
      MergedData <<- NULL
      CalibrationData <<- NULL
      output$calibration <- renderDT({})
    })
  })

  observe({recursiveRefresh()})
  recursiveRefresh <- eventReactive(input$refreshData,{
    isolate({
      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
    })
  })

  observe({recursiveRefresh2()})
  recursiveRefresh2 <- eventReactive(input$refreshData2,{
    isolate({
      output$experiment <- renderDT({
        DF <- MergedData
        datatable(DF)
      })
    })
  })

  observe({recursiveRefresh3()})
  recursiveRefresh3 <- eventReactive(input$refreshData3,{
    isolate({
      output$calibration <- renderDT({
        DF <- CalibrationData
        datatable(DF)
      })
    })
  })

  observe({recursiveExpInfo()})

  recursiveExpInfo <- eventReactive(input$expInfo,{
    updateF7Tabs(session=session, id="tabs", selected = "ExperimentInfo")
  })

  observe({recursiveUploadIntens()})
  recursiveUploadIntens <- eventReactive(input$intensFile,{
    isolate({
      req(input$intensFile)
      tryCatch(
        DF <- read.csv(input$intensFile$datapath, header = TRUE,
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      IntensData <<- DF
      output$intens <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveUploadExpFile()})
  recursiveUploadExpFile <- eventReactive(input$expFile,{
    isolate({
      req(input$expFile)
      tryCatch(
        DF <- read.csv(input$expFile$datapath, header = TRUE,
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      ExpInfo <<- DF
      MergedData <<- DF
      suppressWarnings(rm(CalibrationData, pos = 1))
      output$calibration <- renderDT({})

      output$experiment <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveUploadPrepFile()})
  recursiveUploadPrepFile <- eventReactive(input$prepFile,{
    isolate({
      req(input$prepFile)
      tryCatch(
        DF <- read.csv(input$prepFile$datapath, header = TRUE,
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      CalibrationData <<- DF
      MergedData <<- DF
      output$calibration <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveMerge()})
  recursiveMerge <- eventReactive(input$merge,{
    isolate({
      if (is.null(ExpInfo)) {
        f7Toast(text="Experiment info not found.", position="top", session=session)
      } else if (is.null(IntensData)) {
        f7Toast(text="Intensity data not found.", position="top", session=session)
      } else if (inherits(try(merge(ExpInfo, IntensData,
                                    by.x = input$mergeExp,
                                    by.y = input$mergeIntens, all = TRUE), silent = TRUE), "try-error")) {
        f7Toast(text="Error in the column IDs.", position="top", session=session)
      } else {
        DF <- merge(ExpInfo, IntensData,
                    by.x = input$mergeExp,
                    by.y = input$mergeIntens, all = TRUE)

        MergedData <<- DF
        CalibrationData <<- DF

        output$experiment <- renderDT({
          datatable(DF)
        })
      }
    })
  })

  observe({recursivePrepare()})
  recursivePrepare <- eventReactive(input$prepare,{
    DF <- MergedData
    CalibrationData <<- DF

    output$calibration <- renderDT({
      datatable(DF)
    })

    updateF7Tabs(session=session, id="tabs", selected = "Calibration")
  })


  #Download code
  output$downloadData <- downloadHandler(
    filename = "IntensityData.csv",
    content = function(file) {
      write.csv(IntensData, file, row.names = FALSE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = "MergedData.csv",
    content = function(file) {
      write.csv(MergedData, file, row.names = FALSE)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = "CalibrationData.csv",
    content = function(file) {
      write.csv(CalibrationData, file, row.names = FALSE)
    }
  )

  observe({recursiveCombReps()})
  recursiveCombReps <- eventReactive(input$combReps,{
    isolate({
      Cols <- c(grep("Mean", colnames(MergedData)),
                grep("Median", colnames(MergedData)))
      RES <- NULL
      if(input$colorsBands > 1){
        DF <- MergedData[,c(input$combRepsColSI, input$combRepsColCL)]
        DFuni <- DF[!duplicated(DF),]
        for (i in 1:nrow(DFuni)) {
          sel <- DF[,1] == DFuni[i,1] & DF[,2] == DFuni[i,2]
          tmp <- MergedData[sel, ]
          tmp2 <- tmp[1, ]
          if (input$radioReps == 1) #mean
            tmp2[, Cols] <- colMeans(tmp[, Cols], na.rm = TRUE)
          if (input$radioReps == 2) #median
            tmp2[, Cols] <- apply(tmp[, Cols], 2, median, na.rm = TRUE)
          RES <- rbind(RES, tmp2)
        }
      }else{
        DF <- MergedData[,input$combRepsColSI]
        for (spl in unique(MergedData[, input$combRepsColSI])) {
          tmp <- MergedData[DF == spl, ]
          tmp2 <- tmp[1, ]
          if (input$radioReps == 1) #mean
            tmp2[, Cols] <- colMeans(tmp[, Cols], na.rm = TRUE)
          if (input$radioReps == 2) #median
            tmp2[, Cols] <- apply(tmp[, Cols], 2, median, na.rm = TRUE)
          RES <- rbind(RES, tmp2)
        }
      }
      rownames(RES) <- 1:nrow(RES)
      RES <- RES[order(RES[,input$combRepsColSI]),]
      CalibrationData <<- RES

      output$calibration <- renderDT({
        datatable(RES)
      })
    })
  })

  observe({recursiveReshapeWide()})

  recursiveReshapeWide <- eventReactive(input$reshapeWide,{
    isolate({
      rm.file <- (colnames(CalibrationData) != colnames(MergedData)[1] &
                    colnames(CalibrationData) != input$reshapeCol)
      DF.split <- split(CalibrationData[,rm.file], CalibrationData[,input$reshapeCol])

      N <- length(unique(CalibrationData[,input$reshapeCol]))
      if(N > 1){
        DF <- DF.split[[1]]
        Cols <- c(grep("Mean", colnames(DF)),
                  grep("Median", colnames(DF)))
        Cols <- c(Cols, which(colnames(DF) == input$combRepsColSI))
        for(i in 2:N){
          DF <- merge(DF, DF.split[[i]][,Cols], by = input$combRepsColSI,
                      suffixes = paste0(".", names(DF.split)[c(i-1,i)]))
        }
        CalibrationData <<- DF
      }else{
        DF <- CalibrationData
      }

      output$calibration <- renderDT({
        datatable(DF)
      })
    })
  })

  MODELNUM <- 1

  observe({recursiveRunCali()})

  recursiveRunCali <- eventReactive(input$runCali,{
      output$results <- renderUI({
        f7Block(
          strong = TRUE,
          h3("Results of Calibration Analysis"),
          h4("Calibration model"),
          verbatimTextOutput("modelSummary"), br(),
          plotOutput("plot5"), br(),
          verbatimTextOutput("LOB"),
          verbatimTextOutput("LOD"),
          verbatimTextOutput("LOQ")
        )
      })

      # flush the output and plots
      output$LOB <- renderText({})
      output$LOD <- renderText({})
      output$LOQ <- renderText({})
      output$plot5 <- renderPlot({})

      PATH.OUT <- input$folder
      if (!file.exists(PATH.OUT)) dir.create(PATH.OUT)

      concVar <- input$concVar
      respVar <- paste0("(",input$respVar,")")

      if(input$useLog){
        if(input$chosenModel == "Generalized additive model (gam)"){
          k <- ceiling(length(unique(CalibrationData[,concVar]))/2)
          FORMULA <- paste0(respVar, " ~ s(log10(", concVar, "), k = ", k, ")")
        }else{
          FORMULA <- paste0(respVar, " ~ log10(", concVar, ")")
        }
      }else{
        if(input$chosenModel == "Generalized additive model (gam)"){
          k <- ceiling(length(unique(CalibrationData[,concVar]))/2)
          FORMULA <- paste0(respVar, " ~ s(", concVar, ", k = ", k, ")")
        }else{
          FORMULA <- paste0(respVar, " ~ ", concVar)
        }
      }


      if(input$chosenModel == "Linear model (lm)" && !inherits(try(lm(as.formula(FORMULA), data=CalibrationData), silent = TRUE), "try-error")){
        modelName <- "lm"
      } else if(input$chosenModel == "Local polynomial model (loess)" && !inherits(try(loess(as.formula(FORMULA), data = CalibrationData), silent = TRUE), "try-error")){
        modelName <- "loess"
      } else if(input$chosenModel == "Generalized additive model (gam)" && !inherits(try(gam(as.formula(FORMULA), data = CalibrationData), silent = TRUE), "try-error")){
        modelName <- "gam"
      } else {
        output$results <- renderUI({
          f7Block(
            strong = TRUE,
            h3("Results of Calibration Analysis"),
            h4("Calibration model"),
            verbatimTextOutput("modelSummary")
          )
        })
        output$modelSummary <- renderPrint({print("Calibration can not be performed. Please check the formula.");
          print(paste0("Formula: ",FORMULA))})
        f7Toast(text="Error in the formula!", position="top", session=session)
        updateF7Tabs(session=session, id="tabs", selected = "Results")
        return(NULL)
      }

      f7Toast(text=paste("Fitting the model..."), position="top", session=session)

      SUBSET <- input$subset

      FILENAME <<- paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"), input$analysisName)

      save(CalibrationData, FORMULA, SUBSET, PATH.OUT,
           file = file.path(PATH.OUT, paste0(FILENAME, "_Data.RData")))
      if (input$chosenModel == "Linear model (lm)") {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(lm).Rmd",
                                     package = "LFApp"),
                  to = file.path(PATH.OUT, paste0(FILENAME, "_Analysis.Rmd")))
      } else if (input$chosenModel == "Local polynomial model (loess)") {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(loess).Rmd",
                                     package = "LFApp"),
                  to = file.path(PATH.OUT, paste0(FILENAME, "_Analysis.Rmd")))
      } else if (input$chosenModel == "Generalized additive model (gam)") {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(gam).Rmd",
                                     package = "LFApp"),
                  to = file.path(PATH.OUT, paste0(FILENAME, "_Analysis.Rmd")))
      }

      rmarkdown::render(input = file.path(PATH.OUT, paste0(FILENAME, "_Analysis.Rmd")),
                        output_file = file.path(PATH.OUT, paste0(FILENAME, "_Analysis.html")))

      output$modelSummary <- renderPrint({ fit })

      output$plot5 <- renderPlot({
        modelPlot
      })
      output$LOB <- renderText({
        paste0("Limit of Blank (LOB): ", signif(LOB, 3))
      })
      output$LOD <- renderText({
        paste0("Limit of Detection (LOD): ", signif(LOD, 3))
      })
      output$LOQ <- renderText({
        paste0("Limit of Quantification (LOQ): ", signif(LOQ, 3))
      })

      # Adding the analysis name and model formula to the table
      modelName <- rep(modelName, nrow(CalibrationData))
      modelFormula <- rep(FORMULA, nrow(CalibrationData))
      modelDF <- cbind(modelName, modelFormula, predFunc(CalibrationData))
      colnames(modelDF) <- c(paste0(input$analysisName, ".model"),
                             paste0(input$analysisName, ".formula"),
                             paste0(input$analysisName, ".", input$concVar, ".fit"))
      if(SUBSET != ""){
        subsetIndex <- function (x, subset){
          e <- substitute(subset)
          r <- eval(e, x, parent.frame())
          r & !is.na(r)
        }
        Index <- eval(call("subsetIndex", x = CalibrationData,
                           subset = parse(text = SUBSET)))
        modelDF[!Index,] <- NA
      }
      DF <- cbind(CalibrationData, modelDF)
      CalibrationData <<- DF

      output$calibration <- renderDT({
        datatable(DF)
      })

      MODELNUM <<- MODELNUM + 1

      updateF7Text(session=session, inputId="analysisName", value=paste0("Model", MODELNUM))

      updateF7Tabs(session=session, id="tabs", selected = "Results")
  })

  observe(resetFolder())

  resetFolder <- eventReactive(input$folder,{
    isolate({
      if(substring(input$folder,1,nchar(file.path(fs::path_home()))) != file.path(fs::path_home()))
        updateTextInput(session=session, inputId = "folder", value = file.path(fs::path_home()))
    })
  })

  observe({recursiveOpenReport()})

  recursiveOpenReport <- eventReactive(input$openReport,{
    isolate({
      browseURL(file.path(input$folder, paste0(FILENAME, "_Analysis.html")),
                browser = getOption("browser"))
    })
  })

  # Quantification module ------------------------------------------------------
  observeEvent(input$quanData, {
    quanData <<- read.csv(input$quanData$datapath)
  })

  observeEvent(input$model, {
    calFun <<- readRDS(input$model$datapath)
  })

  observe({predictConc()})

  predictConc <- eventReactive(input$predict, {
    isolate(
      if (!is.null(calFun)) {
        if (input$quanUpload == "Upload Data" && !is.null(quanData)) {
          calConc <- calFun(quanData)
          predictData <<- cbind(quanData, calConc)
          output$quant <- renderDT({
            DF <- predictData
            datatable(DF)
          })
        } else if(input$quanUpload == "Use Intensity Data" && !is.null(IntensData)) {
          calConc <- calFun(IntensData)
          predictData <<- cbind(IntensData, calConc)
          output$quant <- renderDT({
            DF <- predictData
            datatable(DF)
          })
        } else {
          output$quant <- renderDT({})
        }
      }
    )
  })

  #allows user to download prediction
  output$downloadData4 <- downloadHandler(
    filename = "PredictData.csv",
    content = function(file) {
      write.csv(predictData, file, row.names = FALSE)
    }
  )
}
