server <- function(input, output, session) {
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(readxl)
  library(ggplot2)
  source('R/clean_data.R', local = TRUE)
  source('R/calculate_bottle_proportions.R', local = TRUE)


  output$fileUploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  data <- reactive({
    data_path <- input$file$datapath
    data_out <- clean_data(data_path)
    data_out['units'] <- input$flow_choices
    data_out
  }) |>
    bindEvent(input$submit)

  observeEvent(
    input$reset_button, {
      session$reload()
    })

  filtered <- reactive({
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

    list(flow = flow_filtered, sample = sample_filtered, joined = joined_filtered, proportions = proportions
    )
  }) |>
    bindEvent(input$range)


  observe({
    xmin <- min(data()$flow$mins)
    xmax <- max(data()$flow$mins)
    updateSliderInput(inputId = "range", min = xmin, max = xmax,
                      value = c(xmin, xmax))
    if (!all(is.na(data()$sample$values))) {
      appendTab("tabs", tab = tabPanel(
        "EMC",
        textOutput("EMC")
      ))
    }
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
      scale_x_continuous(breaks = scales::breaks_extended(n = 20)) +
      #scale_y_continuous(breaks = scales::breaks_extended(n = 20)) +
      labs(x = 'Time since start (min)', y = paste0('Flowrate (', input$flow_choices, ')'), color = NULL) +
      theme(
        legend.position = 'bottom',
        legend.justification = c("center", "top"),
        legend.background = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 30, size = 14, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14)
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
    bindCache(data(), input$range, input$flow_choices, cache = "session") |>
    bindEvent(input$range, input$flow_choices)

  output$proportions <- renderTable({
    prop_out <- filtered()$proportions[, c("SampleTime", "Proportions")]
    colnames(prop_out) <- c("Sample Times", "Aliquot Volume (mL)")
    prop_out$`Sample Times` <- paste(prop_out$`Sample Times`)
    prop_out$`Aliquot Volume (mL)` <- signif(prop_out$`Aliquot Volume (mL)`, 3)
    prop_out
  }, align = 'c', striped = TRUE, display = c("d", "s", "fg")) |>
    bindCache(data(), input$range, cache = "session")

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Aliquot-Volume-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sapply(filtered()$proportions, signif, digits = 3), file, row.names = FALSE)
    }
  )

  output$EMC <- renderText({
    props <- filtered()$proportions
    sample <- filtered()$sample

    paste('Event Mean Concentration:', signif(as.numeric(props$Proportions%*%sample$values), 3))
  })
}
