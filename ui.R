ui <- fluidPage(
  titlePanel("Flow-Weighting and Compositing Calculator"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "input.submit",
        actionButton("reset_button", "Upload New Data")
      ),
      conditionalPanel(
        "!input.submit",
        fileInput(
          "file", "Choose Excel File",
          multiple = FALSE,
          accept = ".xlsx"
        )
      ),
      selectInput(
        "flow_choices",
        "Flow Units",
        c("",
          "ft³/s",
          "L/s",
          "m³/s"),
        width = "100px"
      ),
      conditionalPanel(
        "input.flow_choices != '' & output.fileUploaded & !input.submit",
        actionButton("submit", "Submit")
      ),
      conditionalPanel(
        "input.submit",
        downloadButton("download_data", "Download .csv"),
        tableOutput("proportions")
      )
    ),
    mainPanel(
      conditionalPanel(
        "input.submit",
        tabsetPanel(
          tabPanel(
            "Flow-Weighting",
            plotOutput("hydrograph", height = 600),
            sliderInput(
              "range",
              "Time range in minutes",
              min = 0,
              max = 1000,
              step = 5,
              value = c(0, 1000)
            )
          ),
          id = "tabs"
        )
      )
    )
  )
)
