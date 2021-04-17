ui <- f7Page(
  title = "Tab Layout",
  f7TabLayout(
    # Maybe the navbar will be removed later.
    navbar = f7Navbar(
      title="LFApp mobile analysis"
    ),
    # Each tab has it's own content
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      f7Tab(
        tabName = "Crop & Segmentation",
        icon = f7Icon("tray_arrow_up"),
        active = TRUE,
        # Upload file block
        f7Block(
          hairlines = FALSE,
          strong = TRUE,
          inset = FALSE,
          f7Radio(inputId= "upload", label="Upload image or choose sample", choices=list("Upload image", "Sample"), selected = "Sample"),
          conditionalPanel(
            condition = "input.upload == 'Upload image'",
            f7File(inputId = 'file1',
                   label = 'Upload Image',
                   placeholder = 'JPEG, PNG, and TIFF are supported',
                   accept = c(
                     "image/jpeg",
                     "image/x-png",
                     "image/tiff",
                     ".jpg",
                     ".png",
                     ".tiff"))
            )
        ),
        f7Accordion(
          f7AccordionItem(
            title = "Rotate Image",
            tagList(
              f7Slider("rotate", label="Angle",
                          min=-45, max=45, value=0, scale=TRUE),
              br(),
              f7Segment(
                f7Button(inputId="rotateCCW", label = "-90"),
                f7Button(inputId="rotateCW", label = "+90"),
                f7Button(inputId="fliphor", label = "FH"),
                f7Button(inputId="flipver", label = "FV")
              )
            )
          )
        ),
        f7Block(
          f7BlockTitle("Set number of strips and number of lines per strip"),
          hairlines = TRUE,
          strong = TRUE,
          inset = FALSE,
          f7Slider("strips", label="Number of strips", min=1, max=10, value=1, scale=TRUE, scaleSteps=9),
          f7Slider("bands", label="Number of lines", min=2, max=6, value=2, scale=TRUE, scaleSteps=4)
        ),
        f7Block(
          f7BlockTitle("Cropping and Segmentation", size="medium"),
          hairlines = TRUE, 
          strong = TRUE,
          inset = FALSE,
          # The content of the tab goes below this
          plotOutput("plot1",
                     click = "plot_click",
                     dblclick = "plot_dblclick",
                     hover = hoverOpts("plot_hover", delay = 5000, clip = TRUE),
                     brush = "plot_brush"),
          h5("Click and drag to select a region of interest. Double click on the selected region to zoom.", align = "center"),
          uiOutput("cropButtons")
        )
      ),
      
      f7Tab(
        tabName = "Background",
        icon = f7Icon("circle_lefthalf_fill"),
        active = FALSE,
        f7Block(
          hairlines = FALSE,
          strong = TRUE,
          inset = FALSE,
          "Select strip: ",
          f7Stepper("selectStrip", label = "", min=1, max=1, value=1, size="small"),
          f7Accordion(
            f7AccordionItem(
              title = "Color image?",
              f7Radio("channel", label="Conversion mode", choices=list("luminance", "gray", "red", "green", "blue"), selected = "luminance"),
            ),
          ),
          f7Radio("invert", label="Lines darker than background?", choices=list("No", "Yes"), selected = "No"),
          f7Radio("thresh", label="Threshold", choices=list("Otsu", "Quantile", "Triangle", "Li"), selected = "Otsu"),
          conditionalPanel(
            condition = "input.thresh == 'Quantile'",
            f7Stepper(inputId = "quantile1",
                     label = "Probability [%]:",
                     value = 99,
                     min = 0,
                     max = 100,
                     step = 0.5,
                     manual = TRUE)
            ),
          conditionalPanel(
            condition = "input.thresh == 'Triangle'",
            f7Stepper(inputId = "tri_offset",
                     label = "Offset:",
                     value = 0.2,
                     min = 0,
                     max = 1,
                     step = 0.1,
                     manual = TRUE)
          ),
          f7Segment(
            f7Button(inputId="threshold", label = "Apply Threshold")
          )
        ),
        uiOutput("threshPlots"),
      ),
      
      f7Tab(
        tabName = "Intensity Data",
        icon = f7Icon("table"),
        active = FALSE,
        f7Block(
          f7Accordion(
            multiCollapse = TRUE,
            f7AccordionItem(
              title = "Upload existing intensity data",
              f7File(inputId = 'intensFile',
                     label = 'Select CSV file',
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"))
            )
        ),
        f7Block(
          strong = TRUE,
          f7Block(
            style = "overflow-x:scroll",
            DTOutput("intens")
          ),
          f7Segment(
            f7DownloadButton("downloadData", label = "Download Data"),
            f7Button("deleteData", color="red", label="Delete Data")
          ),
          f7Button(inputId="quant", label="Switch to Quantification")
          )
        )
      ),
      
       f7Tab(
         tabName = "Quantification",
         icon = f7Icon("gauge"),
         active = FALSE,
         f7Block(
           strong = TRUE,
           h3("Load existing model from file"),
           f7File(inputId = 'model',
                  label = 'Select model',
                  accept = ".rds",
                  placeholder = "RDS file"),
           h3("Predict concentration"),
           f7Button("predict", label = "Predict"),
           h3("Download concentration data"),
           f7DownloadButton("downloadData4", "Download data")
         ),
         DTOutput("quant")
       )
    )
  )
)

server <- function(input, output, session){
  ########## FIRST TAB
  
  options(shiny.maxRequestSize=50*1024^2) #file can be up to 50 mb; default is 5 mb
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL, shiny_img_cropped = NULL, 
                                   shiny_img_final = NULL, Threshold = NULL)
  IntensData <- NULL
  
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
  
  #only executes when Apply Segmentation is clicked
  recursiveSegmentation <- eventReactive(input$segmentation,{
    isolate({
      p <- input$plot_brush
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
            Background.Threshold[count1] <- MultiFlowExt::triangle(img, input$tri_offset)
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
            thr <- MultiFlowExt::triangle(img, input$tri_offset)
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
            Background.Threshold[count1] <- MultiFlowExt::threshold_li(img)
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
            thr <- MultiFlowExt::threshold_li(img, tolerance=input$tri_offset)
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
        BG.method <- matrix(c("Otsu", NA), nrow = 1,
                            ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      if(input$thresh == "Quantile"){
        BG.method <- matrix(c("quantile", input$quantile1),
                            nrow = 1, ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      if(input$thresh == "Triangle"){
        BG.method <- matrix(c("triangle", input$tri_offset), nrow = 1,
                            ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      if(input$thresh == "Li"){
        BG.method <- matrix(c("Li", NA), nrow = 1,
                            ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
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
      updateF7Tabs(session=session, id="tabs", selected = "Intensity Data")
    })
  })
  
  observe({recursiveDelete()})
  recursiveDelete <- eventReactive(input$deleteData,{
    isolate({
      IntensData <<- NULL
      output$intens <- renderDT({})
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
  

  observeEvent(input$intensFile,{
    output$intens <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
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
  
  observe({recursiveQuant()})
  
  recursiveQuant <- eventReactive(input$quant,{
    updateF7Tabs(session=session, id="tabs", selected = "Quantification")
  })
  
  
  #Download code
  output$downloadData <- downloadHandler(
    filename = "IntensityData.csv",
    content = function(file) {
      write.csv(IntensData, file, row.names = FALSE)
    }
  )
  
  # Quantification module ------------------------------------------------------
  predictData <- NULL
  
  observeEvent(input$model, {
    calFun <<- readRDS(input$model$datapath)
  })
  
  observe({predictConc()})
  
  predictConc <- eventReactive(input$predict, {
    isolate(
      if(!is.null(IntensData)) {
        calConc <- calFun(IntensData)
        predictData <<- cbind(IntensData, calConc)
        output$quant <- renderDT({
          DF <- predictData
          datatable(DF)
        })
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
shinyApp(ui = ui, server = server)
