analysis_mobile_ui <- f7Page(
  title = "LFA mobile app",
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
        title = "Crop & Segmentation",
        tabName = "CropSegmentation",
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
          uiOutput("cropButtons"),
          tags$head(
            tags$script("
              $(document).ready(function() {
                var plot = document.getElementById('plot1')

                plot.addEventListener('touchmove', function (e) {
                  var touch = e.changedTouches[0];
                  var mouseEvent = new MouseEvent('mousemove', {
                    view: window,
                    bubbles: true,
                    cancelable: true,
                    screenX: touch.screenX,
                    screenY: touch.screenY,
                    clientX: touch.clientX,
                    clientY: touch.clientY
                  })
                  touch.target.dispatchEvent(mouseEvent);
                  e.preventDefault()
                }, { passive: false });

                plot.addEventListener('touchstart', function(e) {
                  var touch = e.changedTouches[0];
                  var mouseEvent = new MouseEvent('mousedown', {
                    view: window,
                    bubbles: true,
                    cancelable: true,
                    screenX: touch.screenX,
                    screenY: touch.screenY,
                    clientX: touch.clientX,
                    clientY: touch.clientY
                  })
                  touch.target.dispatchEvent(mouseEvent);
                  e.preventDefault()
                }, { passive: false });

                plot.addEventListener('touchstart', function(e) {
                  var touch = e.changedTouches[0];
                  var mouseEvent = new MouseEvent('click', {
                    view: window,
                    bubbles: true,
                    cancelable: true,
                    screenX: touch.screenX,
                    screenY: touch.screenY,
                    clientX: touch.clientX,
                    clientY: touch.clientY
                  })
                  touch.target.dispatchEvent(mouseEvent);
                  e.preventDefault()
                }, { passive: false });

                plot.addEventListener('touchend', function(e) {
                  var touch = e.changedTouches[0];
                  var mouseEvent = new MouseEvent('mouseup', {
                    view: window,
                    bubbles: true,
                    cancelable: true,
                    screenX: touch.screenX,
                    screenY: touch.screenY,
                    clientX: touch.clientX,
                    clientY: touch.clientY
                  })
                  touch.target.dispatchEvent(mouseEvent);
                  e.preventDefault()
                }, { passive: false });
              })
            "),
            tags$style("#plot1 { touch-action: none; }")
          )
        )
      ),
      f7Tab(
        title = "Background",
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
        title = "Intensity Data",
        tabName = "IntensityData",
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
        title = "Experiment Info",
        tabName = "ExperimentInfo",
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
                  f7Checkbox("header", "Header", TRUE),
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
        title = "Calibration",
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
              f7Text("concVar", "Specify column with concentration"),
              f7Checkbox("useLog", "Logarithmize concentration", value=FALSE),
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
        title = "Results",
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
        title = "Quantification",
        tabName = "Quantification",
        icon = f7Icon("gauge"),
        active = FALSE,
        f7Block(
          hairlines = FALSE,
          strong = TRUE,
          inset = FALSE,
          f7Radio(inputId= "quanUpload",
                  label="You can use Intensity Data or upload new data",
                  choices=list("Use Intensity Data", "Upload Data"),
                  selected = "Upload Data"),
          conditionalPanel(
            condition = "input.quanUpload == 'Upload Data'",
            f7File(inputId = 'quanData',
                   label = 'Upload Image',
                   accept = c("csv"))
          )
        ),
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
        f7Block(
          strong = TRUE,
          style = "overflow-x:scroll",
          DTOutput("quant")
        )
      )
    )
  )
)
