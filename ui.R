ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      sliderInput("range",
                  "Time range in minutes",
                  min = 0,
                  max = 1860,
                  value = c(0,1860))
    ),
    mainPanel(
      plotOutput("hydrograph")
    )
  )
)
