server <- function(input, output, session) {
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(readxl)
  library(ggplot2)
  source('R/clean_data.R', local = TRUE)
  source('R/calculate_bottle_proportions.R', local = TRUE)
  source('R/volume_conversion.R', local = TRUE)

  observeEvent(input$reset_button, {session$reload()})

  output$fileUploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  data <- reactive({
    data_path <- input$file$datapath
    data_out <- clean_data(data_path)
    data_out$units <- input$flow_choices
    data_out
  }) |>
    bindEvent(input$submit)

  converted_data <- reactive({
    volume_conversion(data(), input$flow_choices)
  }) |>
    bindEvent(input$submit, input$flow_choices)


  filtered_data <- reactive({

    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined


    flow_filtered <- flow |>
      filter(between(mins, input$range[1], input$range[2]))

    sample_filtered <-  sample |>
      filter(between(mins, input$range[1], input$range[2]))

    joined_filtered <-  joined |>
      filter(between(mins, input$range[1], input$range[2]))

    proportions <- calculate_bottle_proportions(flow_filtered, sample_filtered, joined_filtered, input$composite_vol)

    list(flow = flow_filtered, sample = sample_filtered, joined = joined_filtered, proportions = proportions
    )
  }) |>
    bindEvent(input$submit, input$range, input$composite_vol)

  observeEvent(
    {
      data()
    },
    {
      xmin <- min(data()$flow$mins)
      xmax <- max(data()$flow$mins)
      updateSliderInput(inputId = "range", min = xmin, max = xmax,
                        value = c(round(xmin + 0.5*sd(data()$flow$mins), -1), round(xmax - 0.5*sd(data()$flow$mins), -1)))
    }
  )

  observe({
    if (!all(is.na(converted_data()$sample$values))) {
      appendTab("tabs", tab = tabPanel(
        "EMC",
        textOutput("EMC")
      ))
    }
  })


  output$hydrograph <- renderPlot({
    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined

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
      annotate("rect", xmin = xmin, xmax = input$range[1],
               ymin = ymin, ymax = ymax, alpha = 0.2
      ) +
      annotate("rect", xmin = input$range[2], xmax = xmax,
               ymin = ymin, ymax = ymax, alpha = 0.2
      )
  })

  proportions <- reactive({
    prop_out <- filtered_data()$proportions[, c("SampleTime", "AliquotVolume")]
    colnames(prop_out) <- c("Sample Times", "Aliquot Volume (mL)")
    prop_out$`Sample Times` <- paste(prop_out$`Sample Times`)
    prop_out$`Aliquot Volume (mL)` <- signif(prop_out$`Aliquot Volume (mL)`, 3)
    prop_out
  })


  output$proportions <- renderTable({
    proportions()
  }, align = 'c', striped = TRUE, display = c("d", "s", "fg")) |>
    bindCache(proportions(), cache = "session")

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Aliquot-Volume-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(proportions(), file, row.names = FALSE)
    }
  )

  output$EMC <- renderText({
    props <- filtered_data()$proportions
    sample <- filtered_data()$sample

    paste('Event Mean Concentration:', signif(as.numeric(props$Proportions%*%sample$values), 3))
  })
}
