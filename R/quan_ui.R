quan_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      theme = shinytheme("sandstone"),
      useShinyjs(),
      titlePanel("LFA App quantification"),
      tags$style(type='text/css', "#stop { float:right; }"),
      actionButton("stop", "Quit App"),
      tabsetPanel(id = "tabs",
                  ## Start of Tab Image Editor
                  tabPanel("Cropping and Segmentation", value = "tab1",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("upload",
                                            label = ("1) Upload Image or Choose Sample"),
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
                               h5("2) Set number of strips and number of lines per strip",
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
                                            label = "1) Select strip:",
                                            value = 1,
                                            min = 1,
                                            max = 1,
                                            step = 1,
                                            width = NULL
                               ),
                               hr(style="border-color: black"),
                               h5("2) Select threshold method and apply",
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
                               actionButton("threshold", label = "2) Apply Threshold"), br(),
                               hr(style="border-color: black"),
                               h5("3) Add to Data and go back to 1) or proceed with 4)",
                                  style="font-weight:bold"),
                               actionButton("data", label = "3) Add To Data"), br(),
                               hr(style="border-color: black"),
                               actionButton("showIntensData", label = "4) Switch To Intensity Data")
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
                               h5("Load existing intensity data", style="font-weight:bold"),
                               fileInput("intensFile", "Select CSV file",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")), hr(style="border-color: black"),
                               h5("Download intensity data", style="font-weight:bold"),
                               # actionButton("refreshData", label = "1) Refresh Data"), br(), br(),
                               downloadButton("downloadData", "Download Data"), br(),
                               hr(style="border-color: black"),
                               h5("For restart with new data", style="font-weight:bold"),
                               actionButton("deleteData", label = "Delete Data"), br()
                             ),
                             mainPanel(
                               DTOutput("intens")
                             )
                           )
                  ),
                  tabPanel("Quantification", value = "tab4",
                           sidebarLayout(
                             sidebarPanel(
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

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'LFApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

