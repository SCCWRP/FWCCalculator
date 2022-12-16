source("R/units.R", local = TRUE)

ui <- fluidPage(
  titlePanel("Flow-Weighting and Compositing Calculator"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "!input.submit",
        fileInput(
          "file", "Choose Excel File",
          multiple = FALSE,
          accept = ".xlsx"
        )
      ),
      fluidRow(
        column(
          6,
          selectInput(
            "flow_choices",
            "Flow Units",
            c("", allowed_units),
            width = "150px"
          )
        ),
        column(
          6,
          numericInput(
            "composite_vol",
            "Composite Volume (mL)",
            value = 1000,
            min = 500,
            max = 10000,
            width = "200px"
          )
        )
      ),
      conditionalPanel(
        "input.flow_choices != '' & output.fileUploaded & !input.submit",
        actionButton("submit", "Submit")
      ),
      conditionalPanel(
        "input.submit",
        downloadButton("download_data", "Download .csv"),
        actionButton("reset_button", "Upload New Data"),
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
              value = c(100, 1000)
            )
          ),
          id = "tabs"
        )
      )
    )
  )
)
