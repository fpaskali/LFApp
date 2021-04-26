cal_ui <- function(request) {
  tagList(
    fluidPage(
      theme = shinytheme("sandstone"),
      useShinyjs(),
      titlePanel("LFA App calibration"),
      tags$style(type='text/css', "#stop { float:right; }"),
      actionButton("stop", "Quit App"),
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
                               textInput("folder", "Specify a folder for the analysis results", value=file.path(fs::path_home(), "Documents/LFApp"), 
                                         placeholder=file.path(fs::path_home(), "Documents/LFApp")),
                               hr(style="border-color: black"),
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
                               actionButton("runCali", label = "Run Calibration Analysis"),
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
                               h4("Open analysis report"),
                               actionButton("openReport", label = "Open")
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
                  ) # END OF TAB PANEL
      ) # END OF TAB SET PANEL
    )
  )
}
