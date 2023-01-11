server <- function(input, output, session) {

  observe({
    showModal(
      modalDialog(
        title = "Warning",
        HTML("Press OK to reload the application. All current data will be lost. <br>
              Press Cancel to return to the application."),
        footer = tagList(
          actionButton("ok", "OK"),
          modalButton("Cancel")
        )
      )
    )
  }) |>
    bindEvent(input$reset_button)

  observe({
      session$reload()
  }) |>
    bindEvent(input$ok)


  file_validator <- shinyvalidate::InputValidator$new()

  file_validator$add_rule("file", function(file) {
    if (tools::file_ext(file$datapath) == "xlsx") {
      return(NULL)
    }
    else {
      return("File must be a .xlsx file. Please upload a file in the correct format.")
    }
  })

  file_validator$add_rule("file", function(file) {
    if (length(excel_sheets(file$datapath)) == 2) {
      return(NULL)
    }
    else {
      return("File has more than 2 sheets. Please use the template provided in the Instructions tab.")
    }
  })

  unit_validator <- shinyvalidate::InputValidator$new()

  unit_validator$add_rule("flow_choices", shinyvalidate::sv_required())

  unit_validator$enable()

  observe({
    file_validator$enable()
  }) |>
    bindEvent(req(input$file))


  data <- reactive({
    print("Start data clean")
    flow <- read_excel(input$file$datapath, sheet = 1)
    sample <- read_excel(input$file$datapath, sheet = 2)


    data_out <- clean_data(flow, sample)
    data_out$units <- input$flow_choices
    data_out
  }) |>
    bindEvent(req(file_validator$is_valid() & unit_validator$is_valid()))

  converted_data <- reactive({
    volume_conversion(data(), input$flow_choices)
  }) |>
    bindEvent(req(data()), input$flow_choices)

  ymin <- 0
  ymax <- reactive({
    max(converted_data()$flow$values) + 0.05*(max(converted_data()$flow$values) - ymin)
  }) |>
    bindEvent(converted_data())
  xmin <- 0
  xmax <- reactive({
    max(converted_data()$flow$mins)
  }) |>
    bindEvent(converted_data())

  filtered_data <- reactive({

    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined


    flow_filtered <- flow |>
      filter(between(mins, input$lower_mins, input$upper_mins))

    sample_filtered <-  sample |>
      filter(between(mins, input$lower_mins, input$upper_mins))

    joined_filtered <-  joined |>
      filter(between(mins, input$lower_mins, input$upper_mins))

    proportions <- calculate_bottle_proportions(flow_filtered, sample_filtered, joined_filtered, input$composite_vol)

    list(flow = flow_filtered, sample = sample_filtered, joined = joined_filtered, proportions = proportions)
  }) |>
    bindEvent(converted_data(), input$lower_mins, input$upper_mins, input$composite_vol)

  observe({
      xmin <- min(data()$flow$mins)
      xmax <- max(data()$flow$mins)
      updateNumericInput(inputId = "lower_mins", value = round(xmin + 0.5*sd(data()$flow$mins), -1))
      updateNumericInput(inputId = "upper_mins", value = round(xmax - 0.5*sd(data()$flow$mins), -1))
  }) |>
    bindEvent(data())

  observe({
    if(input$lower_mins < xmin) {
      updateNumericInput(session = session, inputId = "lower_mins", value = xmin)
    } else if (input$upper_mins > xmax()) {
      updateNumericInput(session = session, inputId = "upper_mins", value = xmax())
    }
  }) |>
    bindEvent(input$lower_mins, input$upper_mins)

  output$start_time <- renderText({
    format(input$lower_mins*60 + data()$flow$times[1], "%Y-%m-%d %H:%M:%S")
  }) |>
    bindEvent(data(), input$lower_mins)

  output$end_time <- renderText({
    format(input$upper_mins*60 + data()$flow$times[1], "%Y-%m-%d %H:%M:%S")
  }) |>
    bindEvent(data(), input$upper_mins)



  has_conc <- reactive({
    data()$sample |>
      select(!(contains("mins") | contains("times"))) |>
      summarize(across(.fns = ~ !all(is.na(.x)))) |>
      as.logical() |>
      any()
  }) |>
    bindEvent(data())


  observe({
    insertTab(
      "full_page",
      tab = tabPanel(
        "Flow-Weighting",
        fluidRow(
          column(
            4,
            align = "center",
            br(),
            textOutput("volume1"),
            downloadLink("download_data", "Download Aliquot Volume Table"),
            br(),
            DT::dataTableOutput("proportions", width = "100%")
          ),
          column(
            8,
            br(),
            br(),
            br(),
            br(),
            downloadLink("download_hydrograph", label = "Download Hydrograph"),
            br(),
            plotOutput("hydrograph", height = 500)
          )
        )
      ),
      select = TRUE
    )
  }) |>
    bindEvent(req(data()), once = TRUE)


  observe({
    insertTab(
      "full_page",
      tab = tabPanel(
        "Event Mean Concentration",
        fluidRow(
          column(
            4,
            align = "center",
            br(),
            textOutput("volume2"),
            br(),
            DT::dataTableOutput("conc_table", width = "100%")
          ),
          column(
            8,
            br(),
            strong("Event Mean Concentration"),
            tableOutput("EMC_table"),
            downloadLink("download_pollutograph", label = "Download Pollutograph(s)"),
            br(),
            plotOutput("EMCgraph", height = 500)
          )
        )
      )
    )
  }) |>
    bindEvent(req(has_conc(), data()), once = TRUE)




  hydrograph <- reactive({
    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined


    ggplot() +
      geom_point(data = joined, aes(x = mins, y = values, color = 'Sample Collected')) +
      geom_line(data = flow, aes(x = mins, y = values, color = 'Flow')) +
      coord_cartesian(xlim = c(xmin, xmax()), ylim = c(ymin, ymax()), expand = FALSE) +
      scale_x_continuous(breaks = breaks_extended(n = 20)) +
      labs(x = 'Time since start (min)', y = paste0('Flowrate (', converted_data()$units, ')'), color = NULL) +
      theme(
        text = element_text(size = global_font_size),
        legend.position = 'top',
        legend.justification = c("center", "top"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 0.5)
      ) +
      guides(
        color = guide_legend(
          override.aes = list(shape = c(NA, 16), linetype = c(1, NA))
        )
      )
  }) |>
    bindEvent(converted_data(), xmax(), ymax())

  output$hydrograph <- renderPlot({
    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined


    hydrograph() +
      annotate("rect", xmin = xmin, xmax = input$lower_mins,
               ymin = ymin, ymax = ymax(), alpha = 0.2
      ) +
      annotate("rect", xmin = input$upper_mins, xmax = xmax(),
               ymin = ymin, ymax = ymax(), alpha = 0.2
      )
  })

  proportions <- reactive({
    prop_out <- filtered_data()$proportions[, c("SampleTime", "AliquotVolume")] |>
      rename(`Sample Times` = SampleTime,
             `Aliquot Volume (mL)` = AliquotVolume) |>
      mutate(`Sample Times` = paste(`Sample Times`),
             `Aliquot Volume (mL)` = round(`Aliquot Volume (mL)`),
             `Minutes Since Start` = filtered_data()$sample$mins)

    summary_row <- prop_out |>
      summarize(`Sample Times` = "Total", `Minutes Since Start` = NA, `Aliquot Volume (mL)` = sum(`Aliquot Volume (mL)`))

    prop_out <- prop_out |>
      add_row(summary_row)
    prop_out
  }) |>
    bindEvent(filtered_data())

  # can't output same thing in two html divs, so save same output to 2 vars
  output$volume1 <- output$volume2 <- renderText({
    flow <- converted_data()$flow
    sample <- converted_data()$sample
    joined <- converted_data()$joined

    vol_out <- sum(calculate_bottle_proportions(flow, sample, joined)$Volume)
    paste('Total Hydrograph Volume:', signif(vol_out, 3), substr(converted_data()$units, 1, nchar(converted_data()$units)-2))
  }) |>
    bindEvent(converted_data())


  output$proportions <- DT::renderDataTable({
    proportions()
  }, options = list(searching = FALSE)) |>
    bindEvent(proportions())



  output$download_hydrograph <- downloadHandler(
    filename = function() {
      paste0("Hydrograph-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".png")
    },
    content = function(file) {
      save_plot(file, plot = hydrograph(), device = "png")
    }
  )

  output$download_pollutograph <- downloadHandler(
    filename = function() {
      paste0("Pollutograph-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".png")
    },
    content = function(file) {
      num_cols <- ifelse(length(EMCgraph()) == 1, 1, 2)
      plot <- plot_grid(plotlist=EMCgraph(), ncol = num_cols) + theme(plot.background = element_rect(fill = "white", color = NA))

      save_plot(file, plot = plot, device = "png", ncol = num_cols, nrow = round(length(EMCgraph())/2), base_height = 6.94, base_asp = 1.33)
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("AliquotVolume-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(proportions(), file, row.names = FALSE)
    }
  )


  observe({
    props <- filtered_data()$proportions
    sample <- filtered_data()$sample

    print("EMC table start")

    conc_out <- sample |>
      select(times, !contains("mins")) |>
      rename(`Sample Times` = times) |>
      mutate(`Sample Times` = paste(`Sample Times`))


    EMC_out <- conc_out |>
      summarize(across(.cols = !(contains("mins") | contains("times")), .fns = ~ signif(as.numeric(props$Proportions%*%.x), 3)))


    output$conc_table <- DT::renderDataTable({
      conc_out
    }, options = list(searching = FALSE, autoWidth = TRUE, columnDefs = list(list(width = '155px', targets = 1))))


    output$EMC_table <- renderTable({
      EMC_out
    }, align = "l", striped = TRUE, display = c("d", rep("fg", dim(sample)[2]-2)), width = "100%")
  }) |>
    bindEvent(filtered_data())


  EMCgraph <- reactive({
    flow <- converted_data()$flow
    sample <- converted_data()$sample

    sample <- melt(sample, id.vars = c('mins', 'times'), variable.name = "conc", value.name = "conc_values")


    plot_pollutograph <- function(sample, flow) {
      scale_factor <- max(sample$conc_values)/max(flow$values)

      p <- ggplot() +
        geom_line(data = flow, aes(x = mins, y = values, color = 'Flow')) +
        geom_point(data = sample, aes(x = mins, y = conc_values/scale_factor, color = 'Sample Concentration'), shape = 17) +
        coord_cartesian(xlim = c(0, xmax()), ylim = c(ymin, ymax()), expand = FALSE) +
        scale_x_continuous(breaks = breaks_extended(n = 20)) +
        scale_y_continuous(sec.axis = sec_axis(~ . * scale_factor, name = sample$conc[1])) +
        labs(title = sample$conc[1], x = 'Time since start (min)', y = paste0('Flowrate (', converted_data()$units, ')'), color = NULL) +
        theme(
          text = element_text(size = global_font_size),
          legend.position = 'bottom',
          legend.justification = c("center", "top"),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = 0.5)
        ) +
        guides(
          color = guide_legend(
            override.aes = list(shape = c(NA, 17), linetype = c(1, NA))
          )
        )
      p
    }

    sample_list <- split(sample, sample$conc)
    plot_list <- lapply(sample_list, plot_pollutograph, flow = flow)
    plot_list
  }) |>
    bindEvent(converted_data(), xmax(), ymax())




  output$EMCgraph <- renderPlot({
    plot_list_out <- lapply(EMCgraph(), function(p) p +
                              annotate("rect", xmin = xmin, xmax = input$lower_mins,
                                       ymin = ymin, ymax = ymax(), alpha = 0.2
                              ) +
                              annotate("rect", xmin = input$upper_mins, xmax = xmax(),
                                       ymin = ymin, ymax = ymax(), alpha = 0.2
                              ))


    plot_grid(plotlist=plot_list_out, ncol = ifelse(length(plot_list_out) == 1, 1, 2))

  }, height = function() (ceiling((dim(converted_data()$sample)[2] - 2)/2))*500)

}
