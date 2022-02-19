cal_server <- function( input, output, session ) {
  ###### FIRST TAB
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
  predFunc <- NULL
  CalibrationData <- NULL
  
  startAutosave <- reactiveVal(value=FALSE)
  
  #checks upload for file input
  observe({
    #default: upload image
    if(input$upload == 1){
      output$plot1 <- renderPlot({
        if(is.null(input$file1)) {
          output$rotatePanel <- renderUI({})
        }
        validate(need(!is.null(input$file1), "Must upload a valid jpg, png, or tiff"))
      })
    }
    if(input$upload == 2){
      # using sample image
      img <- readImage(system.file("images", "sample.TIF", package="LFApp"))
      shinyImageFile$shiny_img_origin <- img
      shinyImageFile$shiny_img_cropped <- img
      shinyImageFile$shiny_img_final <- img
      
      shinyImageFile$filename <- "sample.TIF"
      #outputs image to plot1 -- main plot
      output$plot1 <- renderPlot({ EBImage::display(shinyImageFile$shiny_img_final, method = "raster") })
      drawRotatePanel()
    }
  }) # end of observe
  
  # NOTE renameUpload is completely unecessary. The app works without it perfectly.
  
  # #the datapath is different from the one needed to properly recognize photo
  # #so this function renames the file
  # renameUpload <- function(inFile){
  #   if(is.null(inFile))
  #     return(NULL)
  # 
  #   oldNames <- inFile$datapath
  #   newNames <- file.path(dirname(inFile$datapath), inFile$name)
  #   file.rename(from = oldNames, to = newNames)
  #   inFile$datapath <- newNames
  # 
  #   return(inFile$datapath)
  # }
  
  #if they enter a new file, their file will become the new imageFile
  observeEvent(input$file1, {
    shinyImageFile$filename <- input$file1$name
    # img <- readImage(renameUpload(input$file1)) # Commented it, because I think its redundant.
    img <- readImage(input$file1$datapath)
    shinyImageFile$shiny_img_origin <- img
    shinyImageFile$shiny_img_cropped <- img
    shinyImageFile$shiny_img_final <- img
    output$plot1 <- renderPlot({EBImage::display(img, method = "raster")})
    drawRotatePanel()
  })
  
  
  # NOTE This function draws rotation panel.
  drawRotatePanel <- function() {
    output$rotatePanel <- renderUI({
      tagList(
        sliderInput("rotate", "Rotate image",
                    min=-45, max=45, value=0),
        actionButton("rotateCCW", "-90"),
        actionButton("rotateCW", "+90"),
        actionButton("fliphor", "FH"),
        actionButton("flipver", "FV"),
      )
    })
  }
  
  
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
      # For now, resetting only the grid is enough. If there is a need of resseting the image
      # and rotation settings, uncommennt following three lines.
      shinyImageFile$shiny_img_cropped <- shinyImageFile$shiny_img_origin
      shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
      output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
      session$resetBrush("plot_brush")
      updateSliderInput(session, "rotate", value=0)
      shinyjs::disable("segmentation")
    })
  })
  
  
  #prompts shiny to look at recursive crop
  observe({recursiveCrop()})
  
  #only executes when keep is clicked
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
      updateSliderInput(session, "rotate", value=0)
      session$resetBrush("plot_brush")
      shinyjs::enable("reset")
    })
    session$resetBrush("plot_brush")
    shinyjs::disable("segmentation")
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
      shinyjs::enable("reset")
      shinyjs::enable("segmentation")
    })
  })
  
  observe({recursiveSegmentation()})
  
  #only executes when Apply Segmentation is clicked
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
        updateTabsetPanel(session, "tabs", selected = "tab2")
      } else {
        showNotification("Error: The grid is out of bounds", duration = 5, type="error")
      }
    })
  })
  
  ################# END OF THE FIRST TAB
  
  ############### SECOND TAB
  
  observe({
    input$thresh
    updateNumericInput(session, "selectStrip", max=input$strips)
  })
  
  observe({input$channel})
  
  observe({recursiveThreshold()})
  
  # TODO Implement invert color button
  recursiveThreshold <- eventReactive(input$threshold,{
    isolate({
      seg.list <- shinyImageFile$segmentation_list
      i <- input$selectStrip
      if(input$thresh == 2){
        Background <- vector(mode = "list", length = input$bands)
        for(j in 1:input$bands){
          img <- seg.list[[i]][[j]]
          if(colorMode(img) > 0){
            img <- 1-EBImage::channel(img, input$channel)
          }
          if(input$invert) {
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
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
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
          count <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count <- 0
          for(j in Bands){
            count <- count + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
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
      else if(input$thresh == 1){
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
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
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
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
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
      else if(input$thresh == 3){
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
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
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
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
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
      else if(input$thresh == 4){
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
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
              img <- 1 - img
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
              img <- 1-EBImage::channel(img, input$channel)
            }
            if(input$invert) {
              img <- 1 - img
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
  
  observe({recursiveData()})
  
  recursiveData <- eventReactive(input$data,{
    isolate({
      if (!is.null(shinyImageFile$Threshold)) {
        AM <- shinyImageFile$Mean_Intensities
        colnames(AM) <- paste0("Mean", 1:input$bands)
        Med <- shinyImageFile$Median_Intensities
        colnames(Med) <- paste0("Median", 1:input$bands)
        # TODO Here else if will improve the app.
        if(input$thresh == 1){
          BG.method <- matrix(c("Otsu", NA, NA), nrow = 1,
                              ncol = 3, byrow = TRUE)
          colnames(BG.method) <- c("Background", "Offset", "Probability")
        }
        if(input$thresh == 2){
          BG.method <- matrix(c("quantile", NA, input$quantile1),
                              nrow = 1, ncol = 3, byrow = TRUE)
          colnames(BG.method) <- c("Background", "Offset", "Probability")
        }
        if(input$thresh == 3){
          BG.method <- matrix(c("triangle", input$tri_offset, NA), nrow = 1, 
                              ncol = 3, byrow = TRUE)
          colnames(BG.method) <- c("Background", "Offset", "Probability")        
        }
        if(input$thresh == 4){
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
        if(!is.null(shinyImageFile$Threshold))
          shinyImageFile$Threshold <- NULL
        if(!is.null(shinyImageFile$Mean_Intensities))
          shinyImageFile$Mean_Intensities <- NULL
        if(!is.null(shinyImageFile$Median_Intensities))
          shinyImageFile$Median_Intensities <- NULL
        
        # Save the workspace every time you add to intensity data
        # save(shinyImageFile, IntensData, 
        #      ExpInfo, MergedData, fit, 
        #      modelPlot, LOB, LOD, LOQ,
        #      file=file.path(fs::path_home(), "Documents/LFApp/cal_autosave.RData"))
        # showNotification("Workspace saved", duration=2, type="message")
      }
    })
  })
  
  observe({recursiveShowIntensData()})
  recursiveShowIntensData <- eventReactive(input$showIntensData,{
    isolate({
      updateTabsetPanel(session, "tabs", selected = "tab3")
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
    updateTabsetPanel(session, "tabs", selected = "tab4")
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
      updateSelectInput(session = session, "concVar", choices = names(DF))
    })
  })
  
  observe({recursiveMerge()})
  recursiveMerge <- eventReactive(input$merge,{
    isolate({
      if (is.null(ExpInfo)) {
        showNotification("Experiment info not found.", duration=3, type="error")
      } else if (is.null(IntensData)) {
        showNotification("Intensity data not found.", duration=3, type="error")
      } else if (inherits(try(merge(ExpInfo, IntensData,
                                    by.x = input$mergeExp,
                                    by.y = input$mergeIntens, all = TRUE), silent = TRUE), "try-error")) {
        showNotification("Error in the column IDs.", duration=5, type="error")
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
    updateSelectInput(session, "concVar", choices = names(DF))
    updateTabsetPanel(session, "tabs", selected = "tab5")
  })
  
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
    isolate({
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
        if(input$chosenModel == 3){
          k <- ceiling(length(unique(CalibrationData[,concVar]))/2)
          FORMULA <- paste0(respVar, " ~ s(log10(", concVar, "), k = ", k, ")")  
        }else{
          FORMULA <- paste0(respVar, " ~ log10(", concVar, ")")  
        }
      }else{
        if(input$chosenModel == 3){
          k <- ceiling(length(unique(CalibrationData[,concVar]))/2)
          FORMULA <- paste0(respVar, " ~ s(", concVar, ", k = ", k, ")")  
        }else{
          FORMULA <- paste0(respVar, " ~ ", concVar)
        }
      }
      
      if(input$chosenModel == 1 && !inherits(try(lm(as.formula(FORMULA), data=CalibrationData), silent = TRUE), "try-error")){
        modelName <- "lm"
      } else if(input$chosenModel == 2 && !inherits(try(loess(as.formula(FORMULA), data = CalibrationData), silent = TRUE), "try-error")){
        modelName <- "loess"
      } else if(input$chosenModel == 3 && !inherits(try(gam(as.formula(FORMULA), data = CalibrationData), silent = TRUE), "try-error")){
        modelName <- "gam"
      } else {
        output$modelSummary <- renderPrint({print("Calibration can not be performed. Please check the formula.");
          print(paste0("Formula: ",FORMULA))})
        showNotification("Error in the formula!", duration = 5, type="error")
        updateTabsetPanel(session, "tabs", selected = "tab6")
        return(NULL)
      }
      
      info <- showNotification(paste("Fitting the model..."), duration = 0, type="message")
      
      SUBSET <- input$subset
      
      FILENAME <<- paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"), input$analysisName)
      
      save(CalibrationData, FORMULA, SUBSET, PATH.OUT,
           file = file.path(PATH.OUT, paste0(FILENAME, "_Data.RData")))
      if (input$chosenModel == 1) {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(lm).Rmd",
                                     package = "LFApp"),
                  to = file.path(PATH.OUT, paste0(FILENAME, "_Analysis.Rmd")))
      } else if (input$chosenModel == 2) {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(loess).Rmd",
                                     package = "LFApp"),
                  to = file.path(PATH.OUT, paste0(FILENAME, "_Analysis.Rmd")))
      } else if (input$chosenModel == 3) {
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
      
      updateTextInput(session=session, inputId="analysisName", value=paste0("Model", MODELNUM))
      
      removeNotification(info)
      
      updateTabsetPanel(session, "tabs", selected = "tab6")
    })
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
  
  #creates the textbox below plot2 about the plot_brush details and etc
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
  output$intens <- renderDT({
    DF <- IntensData
    datatable(DF)
  })
  output$folder <- renderPrint({
    paste0("Folder for Results: ", parseDirPath(c(wd=fs::path_home()), input$folder))
  })
  
  #allows user to download data
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
  
  #When user clicks the return to command line button
  #stops the shiny app
  # prevents user from quitting shiny using ^C on commandline
  observe({recursiveStop()})
  
  recursiveStop <- eventReactive(input$stop,{
    isolate({
      suppressWarnings(rm(IntensData, pos = 1))
      suppressWarnings(rm(ExpInfo, pos = 1))
      suppressWarnings(rm(MergedData, pos = 1))
      suppressWarnings(rm(CalibrationData, pos = 1))
      stopApp()
    })
  })
  
  # Checking if workspace file exist
  observe({
    if (file.exists(file.path(fs::path_home(), "Documents/LFApp/cal_autosave.RData"))) {
      showModal(modalDialog(
        title = "Old workspace backup found",
        "Do you want to restore previous workspace?",
        h6("If you do not restore the old workspace, it will be overwritten!"),
        footer = tagList(
          actionButton("no_restore", "No"),
          actionButton("restore_work", "Yes")
        )
      ))
    } else {
      startAutosave(TRUE)
    }
  })
  
  observeEvent(input$no_restore, {
    startAutosave(TRUE)
    removeModal()
  })
  
  observeEvent(input$restore_work, {
    load(file=file.path(fs::path_home(), "Documents/LFApp/cal_autosave.RData"))
    
    # Loading the variables properly. Without this the variables are loaded in the observe scope only
    shinyImageFile <<- shinyImageFile
    IntensData <<- IntensData
    ExpInfo <<- ExpInfo
    MergedData <<- MergedData
    fit <<- fit
    modelPlot <<- modelPlot
    LOB <<- LOB
    LOD <<- LOD
    LOQ <<- LOQ
    
    # Loading the image on the first plot
    if (!is.null(shinyImageFile$shiny_img_final))
      output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
    
    # Loading datatables
    output$intens <- renderDT(datatable(IntensData))
    output$experiment <- renderDT(datatable(ExpInfo))
    output$calibration <- renderDT(datatable(MergedData))
    
    # Loading model in results tab
    if (!is.null(fit) && !is.null(LOB) && !is.null(LOD) && !is.null(LOQ)) {
      output$modelSummary <- renderPrint({ fit })
      output$plot5 <- renderPlot({ modelPlot })
      output$LOB <- renderText({ paste0("Limit of Blank (LOB): ", signif(LOB, 3)) })
      output$LOD <- renderText({ paste0("Limit of Detection (LOD): ", signif(LOD, 3)) })
      output$LOQ <- renderText({ paste0("Limit of Quantification (LOQ): ", signif(LOQ, 3)) })
    }
    startAutosave(TRUE)
    removeModal()
  })
  
  # Autosaving every minute
  observe({
    if (startAutosave()) {
      invalidateLater(300000, session)
      if (!file.exists(file.path(fs::path_home(), "Documents/LFApp"))) dir.create(file.path(fs::path_home(), "Documents/LFApp"))
      save(shinyImageFile, IntensData, 
           ExpInfo, MergedData, fit, 
           modelPlot, LOB, LOD, LOQ,
           file=file.path(fs::path_home(), "Documents/LFApp/cal_autosave.RData"))
      showNotification("Workspace saved", duration=2, type="message")
    }
  })
  
  # A function to remove the autosave file if the app was closed properly
  # onStop(function() file.remove(file.path(fs::path_home(), "Documents/LFApp/cal_autosave.RData")))
}
