server <- function(input, output, session) {

  observeEvent(
    input$reset_button,
    {
      showModal(
        modalDialog(
          title = "Warning",
          HTML("Press OK to reload the application and upload new data. All current data will be lost. <br>
               Press Cancel to return to the application."),
          footer = tagList(
            actionButton("ok", "OK"),
            modalButton("Cancel")
          )
        )
      )
    }
  )

  observe({
      session$reload()
  }) |>
    bindEvent(input$ok)

  output$fileUploaded <- reactive({
    print("start check file upload")
    !is.null(input$file)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)


  data_check <- reactive({
    print("start data check")
    data_path <- input$file$datapath
    flow <- read_excel(data_path, sheet = 1)
    sample <- read_excel(data_path, sheet = 2)

    if (!(length(excel_sheets(data_path)) == 2)) {
      showModal(
        modalDialog(
          title = "Error",
          "Excel template does not have 2 sheets. Please try again.",
          footer = modalButton("Ok")
        )
      )
      session$reload()
    }

    list(flow = flow, sample = sample)


  }) |>
    bindEvent(input$submit)

  data <- reactive({
    print("Start data clean")
    req(data_check())

    data_out <- clean_data(data_check())
    data_out$units <- input$flow_choices
    data_out
  }) |>
    bindEvent(data_check())

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

    list(flow = flow_filtered, sample = sample_filtered, joined = joined_filtered, proportions = proportions)
  }) |>
    bindEvent(input$submit, input$range, input$composite_vol)

  observe({
      xmin <- min(data()$flow$mins)
      xmax <- max(data()$flow$mins)
      updateSliderInput(inputId = "range", min = xmin, max = xmax,
                        value = c(round(xmin + 0.5*sd(data()$flow$mins), -1), round(xmax - 0.5*sd(data()$flow$mins), -1)))
  }) |>
    bindEvent(data())


  output$has_conc <- reactive({

    data()$sample |>
      select(starts_with("conc_values")) |>
      summarize(across(.fns = ~ !all(is.na(.x)))) |>
      as.logical() |>
      any()
  })
  outputOptions(output, 'has_conc', suspendWhenHidden=FALSE)

  observe({
    if (input$conc_choices != "") {
      insertTab(
        "full_page",
        tab = tabPanel(
          "EMC",
          fluidRow(
            column(
              4,
              br(),
              textOutput("volume2"),
              tableOutput("EMCtable")
            ),
            column(
              8,
              plotOutput("EMCgraph", height = 500)
            )
          )
        ),
        select = TRUE
      )
    }
    else if (input$submit) {
      insertTab(
        "full_page",
        tab =     tabPanel(
          "Flow-Weighting",
          fluidRow(
            column(
              4,
              br(),
              textOutput("volume1"),
              br(),
              downloadButton("download_data", "Download Results.csv", style = 'display:center-align'),
              tableOutput("proportions")
            ),
            column(
              8,
              plotOutput("hydrograph", height = 500)
            )
          )
        ),
        select = TRUE
      )
    }
  })



  output$hydrograph <- renderPlot({
    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined

    ymin <- 0 # maybe just zero instead?
    ymax <- max(flow$values) + 0.05*(max(flow$values) - ymin)
    xmin <- min(flow$mins)
    xmax <- max(flow$mins)

    ggplot() +
      geom_point(data = joined, aes(x = mins, y = values, color = 'Sample Collected')) +
      geom_line(data = flow, aes(x = mins, y = values, color = 'Flow')) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(ymin, ymax), expand = FALSE) +
      scale_x_continuous(breaks = breaks_extended(n = 20)) +
      labs(x = 'Time since start (min)', y = paste0('Flowrate (', converted_data()$units, ')'), color = NULL) +
      theme(
        legend.position = 'bottom',
        legend.justification = c("center", "top"),
        legend.background = element_blank(),
        legend.text = element_text(size = global_font_size),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 30, size = global_font_size, vjust = 0.5),
        axis.text.y = element_text(size = global_font_size),
        axis.title = element_text(size = global_font_size)
      ) +
      guides(
        color = guide_legend(
          override.aes = list(shape = c(NA, 16), linetype = c(1, NA))
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

  # can't output same thing in two html divs, so save same output to 2 vars
  output$volume1 <- output$volume2 <- renderText({
    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined

    vol_out <- sum(calculate_bottle_proportions(flow, sample, joined)$Volume)
    paste('Total Hydrograph Volume:', signif(vol_out, 3), substr(converted_data()$units, 1, nchar(converted_data()$units)-2))
  })


  output$proportions <- renderTable({
    proportions()
  }, align = 'c', striped = TRUE, display = c("d", "s", "fg")) |>
    bindCache(proportions(), cache = "session")

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Results-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(proportions(), file, row.names = FALSE)
    }
  )


  observe({
    props <- filtered_data()$proportions
    sample <- filtered_data()$sample

    output$EMCtable <- renderTable({
      print("EMC table start")

      EMC_out <- sample |>
        select(times, starts_with("conc_values")) |>
        rename(`Sample Times` = times) |>
        rename_with(.fn = ~ paste0("Concentration ", 1:(dim(sample)[2]-2)), .cols = starts_with("conc_values")) |>
        mutate(`Sample Times` = paste(`Sample Times`))

      summary_row <- EMC_out |>
        summarize(across(.cols = starts_with("Concentration"), .fns = ~ signif(as.numeric(props$Proportions%*%.x), 3)))

      EMC_out <- EMC_out |>
        add_row(`Sample Times` = "Event Mean Concentration", summary_row)

      EMC_out
    }, align = 'c', striped = TRUE, display = c("d", "s", rep("fg", dim(sample)[2]-2)))
  })



  output$EMCgraph <- renderPlot({
    flow <- converted_data()$flow
    sample <- converted_data()$sample

    num_conc <- dim(sample)[2] - 2

    if (num_conc > 1) {
      sample <- melt(sample, id.vars = c('mins', 'times'), variable.name = "conc", value.name = "conc_values")
    } else {
      sample$conc <- "Concentration"
    }


    ymin <- 0
    ymax <- max(flow$values) + 0.05*(max(flow$values) - ymin)
    xmin <- min(flow$mins)
    xmax <- max(flow$mins)



    EMCPlot <- function(sample, flow) {
      scale_factor <- max(sample$conc_values)/max(flow$values)

      p <- ggplot() +
        geom_line(data = flow, aes(x = mins, y = values, color = 'Flow')) +
        geom_point(data = sample, aes(x = mins, y = conc_values/scale_factor, color = 'Sample Concentration'), shape = 17) +
        coord_cartesian(xlim = c(0, xmax), ylim = c(ymin, ymax), expand = FALSE) +
        scale_x_continuous(breaks = breaks_extended(n = 20)) +
        scale_y_continuous(sec.axis = sec_axis(~ . * scale_factor, name = 'TSS')) +
        labs(x = 'Time since start (min)', y = paste0('Flowrate (', converted_data()$units, ')'), color = NULL) +
        theme(
          legend.position = 'bottom',
          legend.justification = c("center", "top"),
          legend.background = element_blank(),
          legend.text = element_text(size = global_font_size),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 30, size = global_font_size, vjust = 0.5),
          axis.text.y = element_text(size = global_font_size),
          axis.title = element_text(size = global_font_size)
        ) +
        guides(
          color = guide_legend(
            override.aes = list(shape = c(NA, 17), linetype = c(1, NA))
          )
        ) +
        annotate("rect", xmin = xmin, xmax = input$range[1],
                 ymin = ymin, ymax = ymax, alpha = 0.2
        ) +
        annotate("rect", xmin = input$range[2], xmax = xmax,
                 ymin = ymin, ymax = ymax, alpha = 0.2
        )
      p
    }

    sample_list <- split(sample, sample$conc)
    plot_list <- lapply(sample_list, EMCPlot, flow = flow)

    plot_grid(plotlist=plot_list, ncol = 1)
  }, height = function() (dim(converted_data()$sample)[2] - 2)*400 )
}
