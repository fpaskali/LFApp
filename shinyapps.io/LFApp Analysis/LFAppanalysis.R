library(shiny)
library(shinythemes)
library(stats)
library(shinyjs)
library(DT)
library(EBImage)
library(fs)
library(rmarkdown)
library(ggplot2)
library(mgcv)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      theme = shinytheme("sandstone"),
      useShinyjs(),
      titlePanel("LFA App analysis"),
      tags$style(type='text/css', "#stop { float:right; }"),
      # actionButton("stop", "Quit App"),
      tabsetPanel(id = "tabs",
                  ## Start of Tab Image Editor
                  tabPanel("Cropping and Segmentation", value = "tab1",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("upload",
                                            label = ("Upload Image or Choose Sample"),
                                            choices = list("Upload Image" = 1,
                                                           "Sample Image" = 2),
                                            selected = 1),
                               conditionalPanel(
                                 condition = "input.upload == 1",
                                 fileInput(inputId = 'file1',
                                           label = 'Upload Image',
                                           placeholder = 'JPEG, PNG, and TIFF are supported',
                                           accept = c(
                                             "image/jpeg",
                                             "image/x-png",
                                             "image/tiff",
                                             ".jpg",
                                             ".png",
                                             ".tiff"))
                               ),
                               uiOutput("rotatePanel"),
                               hr(style="border-color: black"),
                               h5("Set number of strips and number of lines per strip",
                                  style="font-weight:bold"),
                               sliderInput("strips", "Number of strips:",
                                           min = 1, max = 10, value = 1),
                               sliderInput("bands", "Number of lines:",
                                           min = 2, max = 6, value = 2),
                             ),
                             mainPanel(
                               h3('Cropping and Segmentation', align = "center"),
                               plotOutput("plot1",
                                          click = "plot_click",
                                          dblclick = "plot_dblclick",
                                          hover = "plot_hover",
                                          brush = "plot_brush"),
                               br(), 
                               h6("Click and drag to select a region of interest. Double click on the selected region to zoom.", align = "center"),
                               br(),
                               column(6,
                                      actionButton("reset", label = "Reset"),
                                      tags$style(type='text/css', "#reset { display: block; width:70%; margin-left: auto; margin-right:auto;}"),
                               ),
                               column(6, shinyjs::disabled(
                                 actionButton("segmentation", label = "Apply Segmentation")),
                                 tags$style(type='text/css', "#segmentation { display: block; width:70%; margin-left: auto; margin-right:auto;}"),
                               )
                             )
                           )
                  ), # END OF TAB PANEL
                  ## Start of Tab Background Correction
                  tabPanel("Background", value = "tab2",
                           sidebarLayout(
                             sidebarPanel(
                               numericInput(inputId = "selectStrip",
                                            label = "Select strip:",
                                            value = 1,
                                            min = 1,
                                            max = 1,
                                            step = 1,
                                            width = NULL
                               ),
                               hr(style="border-color: black"),
                               h5("Select threshold method",
                                  style="font-weight:bold"),
                               radioButtons("colorImage",
                                            label = ("Color image?"),
                                            choices = list("No" = 1,
                                                           "Yes" = 2),
                                            selected = 1),
                               conditionalPanel(
                                 condition = "input.colorImage == 2",
                                 radioButtons("channel",
                                              label = ("Conversion mode"),
                                              choices = list("luminance",
                                                             "gray",
                                                             "red",
                                                             "green",
                                                             "blue"),
                                              selected = "luminance")
                               ),
                               radioButtons("invert",
                                            label = ("Lines are darker than background?"),
                                            choices = list("No" = FALSE,
                                                           "Yes" = TRUE),
                                            selected = FALSE),
                               radioButtons("thresh",
                                            label = ("Threshold method"),
                                            choices = list("Otsu" = 1,
                                                           "Quantile" = 2,
                                                           "Triangle" = 3,
                                                           "Li" = 4),
                                            selected = 1),
                               conditionalPanel(
                                 condition = "input.thresh == 3",
                                 numericInput(inputId = "tri_offset",
                                              label = "Offset:",
                                              value = 0.2,
                                              min = 0,
                                              max = 1,
                                              step = 0.01,
                                              width = NULL)
                               ),
                               conditionalPanel(
                                 condition = "input.thresh == 2",
                                 numericInput(inputId = "quantile1",
                                              label = "Probability [%]:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              step = 0.1,
                                              width = NULL
                                 )
                               ),
                               actionButton("threshold", label = "Apply Threshold"), br(),
                               hr(style="border-color: black"),
                               actionButton("data", label = "Add To Intensity Data"), br(),
                               hr(style="border-color: black"),
                               actionButton("showIntensData", label = "Switch To Intensity Data")
                             ),
                             mainPanel(
                               HTML(
                                 paste(
                                   h3('Background Correction', align = "center"),
                                   verbatimTextOutput("thresh"),br(),
                                   h4('Signal Intensity Above Background', align = "center"),
                                   plotOutput("plot3"),
                                   h4('Lines After Background Subtraction', align = "center"),
                                   plotOutput("plot4"),
                                   verbatimTextOutput("meanIntens"),
                                   verbatimTextOutput("medianIntens"),
                                   '<br/>','<br/>'
                                 )
                               ),
                               width = 8
                             )
                           )
                  ), # END OF TAB PANEL
                  ## Start of Tab Data
                  tabPanel("Intensity Data", value = "tab3",
                           sidebarLayout(
                             sidebarPanel(
                               h5("You can also upload existing intensity data and go to experiment info", style="font-weight:bold"),
                               fileInput("intensFile", "Select CSV file",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")), hr(style="border-color: black"),
                               h5("Download intensity data", style="font-weight:bold"),
                               # actionButton("refreshData", label = "1) Refresh Data"), br(), br(),
                               downloadButton("downloadData", "Download Data"), br(),
                               hr(style="border-color: black"),
                               actionButton("expInfo", label = "Switch To Experiment Info"),
                               hr(style="border-color: black"),
                               h5("For restart with new data", style="font-weight:bold"),
                               actionButton("deleteData", label = "Delete Data"), br(),
                             ),
                             mainPanel(
                               DTOutput("intens")
                             )
                           )
                  ), # END OF TAB PANEL
                  tabPanel("Experiment Info", value = "tab4",
                           sidebarLayout(
                             sidebarPanel(
                               h5("Upload experiment info or upload existing merged data and go to calibration", style="font-weight:bold"),
                               fileInput("expFile", "Select CSV file",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               # Input: Checkbox if file has header ----
                               checkboxInput("header", "Header", TRUE),
                               # Input: Select separator ----
                               radioButtons("sep", "Separator",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ","),
                               # Input: Select quotes ----
                               radioButtons("quote", "Quote",
                                            choices = c(None = "",
                                                        "Double Quote" = '"',
                                                        "Single Quote" = "'"),
                                            selected = '"'),  hr(style="border-color: black"),
                               h5("Select ID columns and merge datasets", style="font-weight:bold"),
                               textInput("mergeIntens", label = "ID Column Intensity Data", value = "File"),
                               textInput("mergeExp", label = "ID Column Experiment Info", value = "File"),
                               actionButton("merge", label = "Merge With Intensity Data"), br(),
                               hr(style="border-color: black"),
                               h5("Download merged data", style="font-weight:bold"),
                               # actionButton("refreshData2", label = "3) Refresh Data"), br(), br(),
                               downloadButton("downloadData2", "Download Data"), br(),
                               hr(style="border-color: black"),
                               actionButton("prepare", label = "Prepare Calibration"),
                               hr(style="border-color: black"),
                               h5("For restart with new data", style="font-weight:bold"),
                               actionButton("deleteData2", label = "Delete Data"), br(),
                             ),
                             mainPanel(
                               DTOutput("experiment")
                             )
                           )
                  ), # END OF TAB PANEL
                  tabPanel("Calibration", value = "tab5",
                           sidebarLayout(
                             sidebarPanel(
                               # textInput("folder", "Specify a folder for the analysis results", value=file.path(fs::path_home(), "Documents/LFApp"),
                               #           placeholder=file.path(fs::path_home(), "Documents/LFApp")),
                               # hr(style="border-color: black"),
                               #                       h5("Optional: average technical replicates", style="font-weight:bold"),
                               #                       hr(style="border-color: black"),
                               #                       h5("Optional: reshape data from long to wide", style="font-weight:bold"),
                               #                       hr(style="border-color: black"),
                               h5("You can also upload existing data and run the calibration", style="font-weight:bold"),
                               fileInput("prepFile", "Select CSV file",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               hr(style="border-color: black"),
                               radioButtons("radioPrepro",
                                            label = ("Further preprocessing steps:"),
                                            choices = list("None" = 1,
                                                           "Average technical replicates" = 2,
                                                           "Reshape from long to wide" = 3),
                                            selected = 1),
                               conditionalPanel(
                                 condition = "input.radioPrepro == 2",
                                 hr(style="border-color: black"),
                                 textInput("combRepsColSI", label = "Column with sample information:", value = "Sample"),
                                 numericInput(inputId = "colorsBands",
                                              label = "Number of analytes/colors per line:",
                                              value = 1,
                                              min = 1,
                                              max = 5,
                                              step = 1,
                                              width = NULL
                                 ),
                                 conditionalPanel(
                                   condition = "input.colorsBands > 1",
                                   textInput("combRepsColCL", label = "Column with color information:", value = "Color"),
                                 ),
                                 radioButtons("radioReps",
                                              label = ("Choose measure for averaging:"),
                                              choices = list("Mean" = 1,
                                                             "Median" = 2),
                                              selected = 1),
                                 actionButton("combReps", label = "Average Technical Replicates"),
                               ),
                               conditionalPanel(
                                 hr(style="border-color: black"),
                                 condition = "input.radioPrepro == 3",
                                 textInput("reshapeCol", label = "Column:", value = "Color"),
                                 actionButton("reshapeWide", label = "Reshape"),
                               ),
                               hr(style="border-color: black"),
                               h5("Download calibration data", style="font-weight:bold"),
                               # actionButton("refreshData3", label = "3) Refresh Data"), br(), br(),
                               downloadButton("downloadData3", "Download Data"),
                               hr(style="border-color: black"),
                               h5("Calibration", style="font-weight:bold"),
                               textInput("analysisName", label = "Analysis name:", value = "Model1"),
                               radioButtons("chosenModel",
                                            label = "Choose model:",
                                            choices = list("Linear model (lm)" = 1,
                                                           "Local polynomial model (loess)" = 2,
                                                           "Generalized additive model (gam)" = 3),
                                            selected = 1),
                               selectInput("concVar", "Select column with concentration", choices = ""),
                               checkboxInput("useLog", "Logarithmize concentration", value=FALSE),
                               textAreaInput("respVar", label = "Specify the response variable (R expression)"),
                               textAreaInput("subset", label = "Optional: specify subset (logical R expression)"),
                               downloadButton("runCali", label = "Run Calibration Analysis"),
                               hr(style="border-color: black"),
                               h5("For restart with new data", style="font-weight:bold"),
                               actionButton("deleteData3", label = "Delete Data"), br(),
                             ),
                             mainPanel(
                               DTOutput("calibration")
                             )
                           )
                  ), # END OF TAB PANEL
                  tabPanel("Results", value = "tab6",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Save calibration model"),
                               shinyjs::disabled(
                               downloadButton("saveModel", label = "Save Model"))
                             ),
                             mainPanel(
                               h3("Results of Calibration Analysis", style="font-weight:bold"), br(),
                               h4("Calibration model", style="font-weight:bold"),
                               verbatimTextOutput("modelSummary"), br(),
                               plotOutput("plot5"), br(),
                               verbatimTextOutput("LOB"),
                               verbatimTextOutput("LOD"),
                               verbatimTextOutput("LOQ")
                             )
                           )
                  ), # END OF TAB PANEL
                  tabPanel("Quantification", value = "tab4",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("quanUpload",
                                            label = ("You can use Intensity Data or upload new data"),
                                            choices = list("Use Intensity Data" = 1,
                                                           "Upload Data" = 2),
                                            selected = 2),
                               conditionalPanel(
                                 condition = "input.quanUpload == 2",
                                 fileInput(inputId = 'quanData',
                                           label = 'Upload Data',
                                           accept = c(".csv"))
                               ),
                               hr(style="border-color: black"),
                               h5("Load existing model from file", style="font-weight:bold"),
                               fileInput("model", "Select model",
                                         multiple = FALSE,
                                         accept = ".rds",
                                         placeholder = "RDS file"),
                               hr(style="border-color: black"),
                               h5("Predict concentration", style="font-weight:bold"),
                               actionButton("predict", label = "Predict"),
                               hr(style="border-color: black"),
                               h5("Download concentration data", style="font-weight:bold"),
                               downloadButton("downloadData4", "Download data"), br(),
                             ),
                             mainPanel(
                               DTOutput("quant")
                             )
                           )
                  )
      ) # END OF TAB SET PANEL
    )
  )
}

# Thresholding functions
triangle <- function(image, offset = 0.2, breaks = 256) {
  if(!is(image, "Image"))
    stop("'image' must be of class 'Image'!")
  breaks <- as.integer(breaks)
  stopifnot(offset >= 0)
  stopifnot(breaks > 0)
  
  ## compute histogram and extract counts and breaks
  rg <- range(image)
  bins <- breaks
  breaks <- seq(rg[1], rg[2], length = breaks + 1L)
  image.hist <- hist.default(imageData(image), breaks = breaks, plot = FALSE)
  hist.counts <- image.hist$counts
  hist.breaks <- image.hist$breaks
  
  ## centers of bins
  delta <- hist.breaks[2]-hist.breaks[1]
  hist.bins <- hist.breaks[-(bins+1)] + delta/2
  
  ## location of peaks and peak value
  ind.peaks <- which(hist.counts == max(hist.counts))
  ind.first.peak <- ind.peaks[1]
  ind.last.peak <- ind.peaks[length(ind.peaks)]
  peak.height <- hist.counts[ind.first.peak]
  
  ## fist and last bin with positive count
  pos.counts <- which(hist.counts > 0)
  ind.low <- pos.counts[1]
  ind.high <- pos.counts[length(pos.counts)]
  
  if((ind.first.peak - ind.low) < (ind.high - ind.last.peak)){
    ## right tail is longer
    sel <- ind.last.peak:ind.high
    norm.counts <- hist.counts[sel]/peak.height
    norm.bins <- (1:length(sel))/length(sel)
    distances <- (1-norm.counts)*(1-norm.bins)/sqrt((1-norm.counts)^2 + (1-norm.bins)^2)
  }else{
    ## left tail is longer
    sel <- ind.low:ind.first.peak
    norm.counts <- hist.counts[sel]/peak.height
    norm.bins <- (1:length(sel))/length(sel)
    distances <- (1-norm.counts)*norm.bins/sqrt((1-norm.counts)^2 + norm.bins^2)
  }
  ind.max <- which.max(distances)
  
  hist.bins[sel[ind.max]] + offset
}

threshold_li <- function(image, tolerance=NULL, initial_guess=NULL, iter_callback=NULL) {
  # For Li's algorithm to work, the image should be positive
  image_min <- min(image)
  image <- image - image_min
  # Tolerance has to be positive or there is a risk of while loop to be infinite.
  if (is.null(tolerance)) {
    tolerance <- abs(min(diff(as.vector(image))) / 2)
  }
  # Initial estimate for iterations
  if (is.null(initial_guess)) {
    t_next <- mean(image)
  } else if (class(initial_guess)=="function"){
    t_next <- initial_guess(image)
  } else if (is.numeric(initial_guess)) {
    t_next <- initial_guess - image_min
    image_max <- max(image) + image_min
    if (!t_next>0 & !t_next<max(image)) {
      stop(sprintf("The threshold_li must be greater than 0 and lesser than max
                   value of the image. threshold_li is %s", threshold_li))
    } else {
      stop("The initial_guess has incorrect class. It should be numeric or a 
           function that returns numeric value.")
    }
  }
  # The difference between t_next and t_curr should be equal to the tolerance.
  t_curr <- tolerance * (-2)
  
  if (!is.null(iter_callback)) {
    iter_callback(t_next + image_min)
  } 
  
  # Stop the iteration when the difference between the new and old threshold
  # value is less than the tolerance
  while(abs(t_next - t_curr) > tolerance) {
    t_curr <- t_next
    foreground <- (image > t_curr)
    mean_fore <- mean(image[foreground])
    mean_back <- mean(image[!foreground])
    
    t_next <- ((mean_back - mean_fore) /
                 (log(mean_back) - log(mean_fore)))
    
    if (!is.null(iter_callback)) {
      iter_callback(t_next + image_min)
    }
  }
  threshold <- t_next + image_min
  threshold
}


app_server <- function( input, output, session ) {
  ###### FIRST TAB
  
  options(shiny.maxRequestSize=50*1024^2) #file can be up to 50 mb; default is 5 mb
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL, shiny_img_cropped = NULL,
                                   shiny_img_final = NULL, Threshold = NULL)
  
  IntensData <- NULL
  ExpInfo <- NULL
  MergedData <- NULL
  fit <- NULL
  modelPlot <- NULL
  LOB <- NULL
  LOD <- NULL
  LOQ <- NULL
  calFun <- NULL
  predFun <- NULL
  
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
      img <- readImage("sample.TIF")
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
      AM <- shinyImageFile$Mean_Intensities
      colnames(AM) <- paste0("Mean", 1:input$bands)
      Med <- shinyImageFile$Median_Intensities
      colnames(Med) <- paste0("Median", 1:input$bands)
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
      output$calibration <- renderDT({
        datatable(DF)
      })
      updateSelectInput(session = session, "concVar", choices = names(DF))
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
  
  output$runCali <- downloadHandler(
    filename = "Analysis Report.html",
    content = function(file) {
      # flush the output and plots
      output$LOB <- renderText({})
      output$LOD <- renderText({})
      output$LOQ <- renderText({})
      output$plot5 <- renderPlot({})
    
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
        shinyjs::disable("saveModel")
        updateTabsetPanel(session, "tabs", selected = "tab6")
        return(NULL)
      }
      
      info <- showNotification(paste("Fitting the model..."), duration = 0, type="message")
      
      SUBSET <- input$subset
      
      # FILENAME <<- paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"), input$analysisName)
      
      # save(CalibrationData, FORMULA, SUBSET, PATH.OUT,
      #      file = paste0(PATH.OUT,"/", FILENAME, "_Data.RData"))
      
        

      if (input$chosenModel == 1) {
        src <- normalizePath("CalibrationAnalysis(lm).Rmd")
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src,
                  "ReportAnalysis.Rmd", overwrite = TRUE)
      } else if (input$chosenModel == 2) {
        src <- normalizePath("CalibrationAnalysis(loess).Rmd")
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src,
                  "ReportAnalysis.Rmd", overwrite = TRUE)
      } else if (input$chosenModel == 3) {
        src <- normalizePath("CalibrationAnalysis(gam).Rmd")
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src,
                  "ReportAnalysis.Rmd", overwrite = TRUE)
      }
      out <- rmarkdown::render("ReportAnalysis.Rmd", html_document())
      file.rename(out,file)
    
      # load(file = paste0(PATH.OUT, "/", FILENAME, "Results.RData")) # This line is not necessary, because the parameters are still loaded in the environment.
      
      predFunc <<- predFunc # make predFunc global for save model feature
      
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
      if (input$chosenModel == 2) {
        modelDF <- cbind(modelName, modelFormula, fit$fitted)
      } else {
        modelDF <- cbind(modelName, modelFormula, fit$fitted.values)
      }
      colnames(modelDF) <- c(paste0(input$analysisName, ".model"), 
                             paste0(input$analysisName, ".formula"), 
                             paste0(input$analysisName, ".fit"))
      CalibrationData <<- cbind(CalibrationData, modelDF)
      output$calibration <- renderDT({
        datatable(CalibrationData)
      })
      
      MODELNUM <<- MODELNUM + 1
      
      updateTextInput(session=session, inputId="analysisName", value=paste0("Model", MODELNUM))
      
      removeNotification(info)
      shinyjs::enable("saveModel")
      
      updateTabsetPanel(session, "tabs", selected = "tab6")
    })
  
  
  # observe(resetFolder())
  
  # resetFolder <- eventReactive(input$folder,{
  #   isolate({
  #     if(substring(input$folder,1,nchar(file.path(fs::path_home()))) != file.path(fs::path_home()))
  #       updateTextInput(session=session, inputId = "folder", value = file.path(fs::path_home())) 
  #   })
  # })
  # 

  output$saveModel <- downloadHandler(
    filename= paste0("Model.rds"),
    content = function(file) {
      saveRDS(object=predFunc, file)
    }
  )
  
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
        if (input$quanUpload == 2 && !is.null(quanData)) {
          calConc <- calFun(quanData)
          predictData <<- cbind(quanData, calConc)
          output$quant <- renderDT({
            DF <- predictData
            datatable(DF)
          })
        } else if(input$quanUpload == 1 && !is.null(IntensData)) {
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

shinyApp(app_ui, app_server)
