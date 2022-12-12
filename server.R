server <- function(input, output) {
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(readxl)
  library(ggplot2)
  source('R/clean_data.R', local = TRUE)

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })

  output$hydrograph <- renderPlot({
    data_path <- 'data/EMC_Template-preprocess.xlsx'
    cleaned_data <- clean_data(data_path)
    flow <- cleaned_data$flow
    sample <- cleaned_data$sample
    joined <- cleaned_data$joined

    ggplot() +
      geom_point(data = joined, aes(x = mins, y = values_flow, color = 'Sample')) +
      geom_line(data = flow, aes(x = mins, y = values, color = 'Flow')) +
      ylim(min(flow$values) - sd(flow$values),max(flow$values) + sd(flow$values)) +
      labs(title = 'Hydrograph', x = 'Time since start (min)', y = 'Flowrate', color = NULL) +
      theme(legend.position = c(.925,.95), legend.background = element_blank(), legend.key = element_blank()) +
      guides(color = guide_legend(override.aes = list(shape = c(NA,16), linetype = c(1,NA))))
  })
}
