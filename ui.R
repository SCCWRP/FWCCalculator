ui <- navbarPage(
  "EMC Calculator",
  tabPanel("Upload"),
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
              min = xmin,
              max = xmax,
              value = c(xmin, xmax)
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
