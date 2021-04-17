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
          f7Button(inputId="expInfo", label="Switch to experiment info")
          )
        )
      ),
      f7Tab(
        tabName = "Experiment Info",
        icon = f7Icon("info_circle"),
        active = FALSE,
        f7Block(
          f7Accordion(
            multiCollapse = TRUE,
            f7AccordionItem(
              title = "Upload experiment info or upload existing merged data",
              f7Accordion(
                f7File(inputId = 'expFile',
                       label = 'Select CSV file',
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
                f7AccordionItem(
                  title = "CSV options",
                  f7checkBox("header", "Header", TRUE),
                  f7Radio("sep", "Separator",
                          choices = c("Comma ( , )",
                                      "Semicolon ( ; )",
                                      "Tab ( \\t )"),
                          selected = "Comma ( , )"),
                  f7Radio("quote", "Quote",
                          choices = c("None",
                                      'Double Quote ( " )',
                                      "Single Quote ( ' )"),
                          selected = "None"))
                )
              ),
            f7AccordionItem(
              title = "Merge datasets",
              "Select ID columns and merge datasets",
              f7Text("mergeIntens", label = "ID Column Intensity Data", value = "File"),
              f7Text("mergeExp", label = "ID Column Experiment Info", value = "File"),
              f7Segment(
                f7Button("merge", label = "Merge With Intensity Data")
              )
            ),
          ),
          f7Block(
            strong = TRUE,
            f7Block(
              strong = TRUE,
              style = "overflow-x:scroll",
              DTOutput("experiment")
            ),
            f7Segment(
              f7DownloadButton("downloadData2", label = "Download Data"),
              f7Button("deleteData2", color="red", label="Delete Data")

            ),
            f7Block(
              f7Button("prepare", label = "Prepare Calibration")
            )
          )
        )
      ),
      f7Tab(
        tabName = "Calibration",
        icon = f7Icon("graph_square"),
        active = FALSE,
        f7Block(
          f7Accordion(
            multiCollapse=TRUE,
            f7AccordionItem(
              title = "Upload existing data for calibration",
              f7File(inputId = 'prepFile',
                     label = 'Select CSV file',
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"))
            ),
            f7AccordionItem(
              title = "Furter preprocessing steps",
              f7Radio("radioPrepro",
                      label = "",
                      choices = c("None",
                                  "Average technical replicates",
                                  "Reshape from long to wide"),
                      selected = "None"),
              conditionalPanel(
                condition = "input.radioPrepro == 'Average technical replicates'",
                f7Text("combRepsColSI", label = "Column with sample information:", value = "Sample"),
                f7Stepper(inputId = "colorsBands",
                          label = "Number of analytes/colors per line:",
                          value = 1,
                          min = 1,
                          max = 5,
                          step = 1,
                ),
                conditionalPanel(
                  condition = "input.colorsBands > 1",
                  f7Text("combRepsColCL", label = "Column with color information:", value = "Color"),
                ),
                f7Radio("radioReps",
                        label = ("Choose measure for averaging:"),
                        choices = list("Mean",
                                       "Median"),
                        selected = "Mean"),
                f7Segment(
                  f7Button("combReps", label = "Average Technical Replicates")
                )
              ),
              conditionalPanel(
                condition = "input.radioPrepro == 'Reshape from long to wide'",
                f7Text("reshapeCol", label = "Column:", value = "Color"),
                f7Segment(
                  f7Button("reshapeWide", label = "Reshape")
                )
              )
            ),
            f7AccordionItem(
              title = "Calibration",
              open = TRUE,
              f7Text("analysisName","Analysis name:", value="Model1"),
              f7Radio("chosenModel",
                      label = "Choose model",
                      choices = list("Linear model (lm)",
                                     "Local polynomial model (loess)",
                                     "Generalized additive model (gam)"),
                      selected = "Linear model (lm)"),
              f7TextArea("respVar", label = "Specify response variable (R expresssion)"),
              f7TextArea("subset", label = "Optional: specify subset (logical R expression)"),
              f7Picker("concVar", label = "Specify column with concentration", choices=""),
              # f7Text("concVar", "Specify column with concentration"),
              f7checkBox("useLog", "Logarithmize concentration", value=FALSE),
              f7Block(
                strong = TRUE,
                f7Block(
                  strong = TRUE,
                  style = "overflow-x:scroll",
                  verbatimTextOutput("folder"),
                  DTOutput("calibration")
                ),
                f7Segment(
                  f7DownloadButton("downloadData3", label = "Download Data"),
                  f7Button("deleteData3", color="red", label="Delete Data")
                )
              ),
              f7Segment(
                f7Text("folder","Working directory:", value=file.path(fs::path_home(), "Documents/LFApp"), 
                       placeholder=file.path(fs::path_home(), "Documents/LFApp")),
                f7Button("runCali", label = "Run Calibration Analysis")
              )
            )
          ),
        )
      ),
      f7Tab(
        tabName = "Results",
        icon = f7Icon("doc_text_search"),
        active = FALSE,
        f7Block(
          strong = TRUE, 
          h3("Open analysis report"),
          f7Button("openReport", label = "Open")
        ),
        uiOutput("results")
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
  observeEvent(input$intensFile,{
    output$intens <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
  })
  observeEvent(input$expFile,{
    output$experiment <- renderDT({})
    suppressWarnings(rm(ExpInfo, pos = 1))
    suppressWarnings(rm(MergedData, pos = 1))
  })
  observeEvent(input$prepFile,{
    output$calibration <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
    suppressWarnings(rm(ExpInfo, pos = 1))
    suppressWarnings(rm(MergedData, pos = 1))
  })
  
  observe({recursiveExpInfo()})
  
  recursiveExpInfo <- eventReactive(input$expInfo,{
    updateF7Tabs(session=session, id="tabs", selected = "Experiment Info")
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
      output$calibration <- renderDT({
        datatable(DF)
      })
      updateF7Picker("concVar", choices=names(DF))
    })
  })
  
  observe({recursiveMerge()})
  recursiveMerge <- eventReactive(input$merge,{
    isolate({
      DF <- merge(ExpInfo, IntensData,
                  by.x = input$mergeExp,
                  by.y = input$mergeIntens, all = TRUE)
      
      MergedData <<- DF
      CalibrationData <<- DF
      
      output$experiment <- renderDT({
        datatable(DF)
      })
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
    isolate({
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
      } else if(input$chosenModel == "Local polynomial model (loess)" && !inherits(try(loess(as.formula(FORMULA), data = combinedData.red), silent = TRUE), "try-error")){
      } else if(input$chosenModel == "Generalized additive model (gam)" && !inherits(try(gam(as.formula(FORMULA), data = combinedData.red), silent = TRUE), "try-error")){
      } else {
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
           file = paste0(PATH.OUT,"/", FILENAME, "_Data.RData"))
      if (input$chosenModel == "Linear model (lm)") {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(lm).Rmd",
                                     package = "LFApp"),
                  to = paste0(PATH.OUT, "/", FILENAME, "_Analysis.Rmd"))
      } else if (input$chosenModel == "Local polynomial model (loess)") {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(loess).Rmd",
                                     package = "LFApp"),
                  to = paste0(PATH.OUT, "/", FILENAME, "_Analysis.Rmd"))
      } else if (input$chosenModel == "Generalized additive model (gam)") {
        file.copy(from = system.file("markdown", "CalibrationAnalysis(gam).Rmd",
                                     package = "LFApp"),
                  to = paste0(PATH.OUT, "/", FILENAME, "_Analysis.Rmd"))
      }
      
      rmarkdown::render(input = paste0(PATH.OUT, "/", FILENAME, "_Analysis.Rmd"),
                        output_file = paste0(PATH.OUT, "/", FILENAME, "_Analysis.html"))
      
      # load(file = paste0(PATH.OUT, "/", FILENAME, "Results.RData")) # This line is not necessary, because the parameters are still loaded in the environment.
      
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
      analysisName <- rep(input$analysisName, nrow(CalibrationData))
      modelFormula <- rep(FORMULA, nrow(CalibrationData))
      CalibrationData <- cbind(CalibrationData, analysisName, modelFormula)
      output$calibration <- renderDT({
        datatable(CalibrationData)
      })
      
      MODELNUM <<- MODELNUM + 1
      
      updateF7Text(session=session, inputId="analysisName", value=paste0("Model", MODELNUM))
      
      updateF7Tabs(session=session, id="tabs", selected = "Results")
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
      browseURL(paste0(input$folder, "/", FILENAME, "_Analysis.html"),
                browser = getOption("browser"))
    })
  })
  
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
