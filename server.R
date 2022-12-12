server <- function(input, output) {
  output$hydrograph <- renderPlot({
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
  })

  output$proportions <- renderTable({
    flow_filtered <- flow |>
      filter(between(mins, input$range[1], input$range[2]))

    sample_filtered <-  sample |>
      filter(between(mins, input$range[1], input$range[2]))

    joined_filtered <-  joined |>
      filter(between(mins, input$range[1], input$range[2]))

    proportions <- calculate_bottle_proportions(flow_filtered, sample_filtered, joined_filtered)
    colnames(proportions) <- c('Bottle Number', 'Proportions (mL)')
    proportions
  }, digits = 1, align = 'c', striped = TRUE)
}
