ui <- navbarPage(
  "EMC Calculator",
  tabPanel(
    "Upload",
    fileInput(
      "file", "Choose Excel File",
      multiple = FALSE,
      accept = ".xlsx"
    ),
    hr(),
    actionButton("run_graph", "Load Hydrograph")
  ),
  tabPanel(
    "Hydrograph",
    fluidRow(
      column(
        8,
        plotOutput("hydrograph"),
        fluidRow(
          column(
            12,
            offset = 3,
            sliderInput(
              "range",
              "Time range in minutes",
              min = 0,
              max = 1000,
              value = c(0, 1000)
            )
          )
        )
      ),
      column(
        4,
        tableOutput("proportions")
      )
    )
  )
)
