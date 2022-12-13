server <- function(input, output) {
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(readxl)
  library(ggplot2)
  source('R/clean_data.R', local = TRUE)
  source('R/calculate_bottle_proportions.R', local = TRUE)


  data <- reactive({
    data_path <- input$file$datapath
    clean_data(data_path)
  }) |>
    bindEvent(input$run_graph)

  observe({
    xmin <- min(data()$flow$mins)
    xmax <- max(data()$flow$mins)
    updateSliderInput(inputId = "range", min = xmin, max = xmax,
                      value = c(xmin, xmax))
  })


  output$hydrograph <- renderPlot({
    flow <- data()$flow
    sample <- data()$sample
    joined <- data()$joined

    ymin <- min(flow$values) - sd(flow$values) # maybe just zero instead?
    ymax <- max(flow$values) + sd(flow$values)
    xmin <- min(flow$mins)
    xmax <- max(flow$mins)

    ggplot() +
      geom_point(data = joined, aes(x = mins, y = values, color = 'Sample')) +
      geom_line(data = flow, aes(x = mins, y = values, color = 'Flow')) +
      ylim(ymin, ymax) +
      labs(x = 'Time since start (min)', y = 'Flowrate', color = NULL) +
      theme(
        legend.position = 'top',#c(0.5, 1),
        legend.justification = c("center", "top"),
        legend.background = element_blank(),
        legend.key = element_blank()
      ) +
      guides(
        color = guide_legend(
          override.aes = list(shape = c(NA,16), linetype = c(1,NA))
        )
      ) +
      annotate(
        "rect",
        xmin = xmin,
        xmax = input$range[1],
        ymin = ymin,
        ymax = ymax,
        alpha = 0.2
      ) +
      annotate(
        "rect",
        xmin = input$range[2],
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        alpha = 0.2
      )
  }) |>
    bindCache(data(), input$range)

  output$proportions <- renderTable({
    flow <- data()$flow
    sample <- data()$sample
    joined <- data()$joined

    flow_filtered <- flow |>
      filter(between(mins, input$range[1], input$range[2]))

    sample_filtered <-  sample |>
      filter(between(mins, input$range[1], input$range[2]))

    joined_filtered <-  joined |>
      filter(between(mins, input$range[1], input$range[2]))

    proportions <- calculate_bottle_proportions(flow_filtered, sample_filtered, joined_filtered)
    colnames(proportions) <- c('Bottle Number', 'Proportions (mL)')
    proportions
  }, align = 'c', striped = TRUE) |>
    bindCache(data(), input$range)
}
