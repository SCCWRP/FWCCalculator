ui <- fluidPage(
  titlePanel("EMC Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file", "Choose Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      actionButton("run_graph", "Load Hydrograph")
    ),
    mainPanel(
      fluidRow(
        column(
          8,
          conditionalPanel(
            "input.run_graph",
            tabsetPanel(
              tabPanel(
                "Hydrograph",
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
        ),
        column(
          4,
          tableOutput("proportions"),
          conditionalPanel(
            "input.run_graph",
            downloadButton("download_data", "Download")
          )
        )
      )
    )
  )
)
