core_mobile_ui <- f7Page(
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
          uiOutput("cropButtons")
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
          )
          )
        )
      )
    )
  )
)
