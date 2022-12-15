ui <- fluidPage(
  titlePanel("Flow-Weighting and Compositing Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file", "Choose Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      actionButton("run_graph", "Load Hydrograph"),
      conditionalPanel(
        "input.run_graph",
        downloadButton("download_data", "Download"),
        tableOutput("proportions")
      )
    ),
    mainPanel(
      conditionalPanel(
        "input.run_graph",
        tabsetPanel(
          tabPanel(
            "Flow-Weighting",
            plotOutput("hydrograph"),
            sliderInput(
              "range",
              "Time range in minutes",
              min = 0,
              max = 1000,
              value = c(0, 1000)
            )
          ),
          id = "tabs"
        )
      )
    )
  )
)
