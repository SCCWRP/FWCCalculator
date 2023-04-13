server <- function(input, output, session) {
  print("server start")

  ######## Reset button logic
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
  ########

  ######## File Validation
  file_validator <- shinyvalidate::InputValidator$new()

  file_validator$add_rule("file", function(file) is_correct_filetype(file))

  file_validator$add_rule("file", function(file) has_two_sheets(file))

  file_validator$add_rule("file", function(file) has_two_columns(file))

  file_validator$add_rule("file", function(file, sheet) has_headers(file, sheet), sheet = 1)
  file_validator$add_rule("file", function(file, sheet) has_headers(file, sheet), sheet = 2)

  file_validator$add_rule("file", function(file, sheet) has_correct_date_format(file, sheet), sheet = 1)
  file_validator$add_rule("file", function(file, sheet) has_correct_date_format(file, sheet), sheet = 2)

  file_validator$add_rule("file", function(file) has_sample_times_in_range(file))

  file_validator$add_rule("file", function(file, sheet) has_correct_measurement_format(file, sheet), sheet = 1)
  file_validator$add_rule("file", function(file, sheet) has_correct_measurement_format(file, sheet), sheet = 2)

  file_validator$add_rule("file", function(file, sheet) has_no_missing_values(file, sheet), sheet = 1)
  file_validator$add_rule("file", function(file, sheet) has_no_missing_values(file, sheet), sheet = 2)

  file_validator$add_rule("file", function(file, sheet) has_no_negative_values(file, sheet), sheet = 1)
  file_validator$add_rule("file", function(file, sheet) has_no_negative_values(file, sheet), sheet = 2)

  observe({
    file_validator$enable()

    shinyjs::toggleState("submit", file_validator$is_valid())
    shinyjs::toggleState("composite_vol", file_validator$is_valid())
    shinyjs::toggleState("lower_mins", file_validator$is_valid())
    shinyjs::toggleState("flow_units", file_validator$is_valid())
    shinyjs::toggleState("upper_mins", file_validator$is_valid())
    shinyjs::toggleState("redraw_graph", file_validator$is_valid())
  }) |>
    bindEvent(req(input$file))

  ########

  ######## Initial data input from excel file, set some global values related to data()
  data <- reactive({
    print("Start data clean")
    flow <- readxl::read_excel(input$file$datapath, sheet = 1)
    sample <- readxl::read_excel(input$file$datapath, sheet = 2)

    data_out <- clean_data(flow, sample)

    xmin <- min(data_out$flow$mins)
    xmax <- max(data_out$flow$mins)
    # set the initial start and end filters so that filtering functionality is visible/apparent
    # updateNumericInput(inputId = "lower_mins", value = round(xmin + 0.5*stats::sd(data_out$flow$mins), -1))
    # updateNumericInput(inputId = "upper_mins", value = round(xmax - 0.5*stats::sd(data_out$flow$mins), -1))
    # keep at 0 instead
    updateNumericInput(inputId = "lower_mins", value = 0)
    updateNumericInput(inputId = "upper_mins", value = xmax)


    data_out
  }) |>
    bindEvent(input$submit)

  # delay so that all filtered data has time to get updated correctly before drawing graphs
  observe({
    shinyjs::delay(10, shinyjs::click("redraw_graph"))
  }) |>
    bindEvent(data())

  # graph limits
  ymin <- 0
  ymax <- reactive({
    max(data()$flow$flow_values) + 0.05*(max(data()$flow$flow_values) - ymin)
  }) |>
    bindEvent(input$submit)
  xmin <- 0
  xmax <- reactive({
    max(data()$flow$mins)
  }) |>
    bindEvent(input$submit)

  ########


  # update numeric inputs on invalid values
  observe({
    if (input$lower_mins < xmin | is.na(input$lower_mins)) {
      updateNumericInput(session = session, inputId = "lower_mins", value = xmin)
    }
    if (input$upper_mins > xmax() | is.na(input$upper_mins)) {
      updateNumericInput(session = session, inputId = "upper_mins", value = xmax())
    }
    if (input$lower_mins > xmax()) {
      updateNumericInput(session = session, inputId = "lower_mins", value = xmax() - 30)
      updateNumericInput(session = session, inputId = "upper_mins", value = xmax())
    }
    if (input$lower_mins > input$upper_mins) {
      updateNumericInput(session = session, inputId = "upper_mins", value = ifelse(input$lower_mins + 30 <= xmax(), input$lower_mins + 30, xmax()))
    }
  }) |>
    bindEvent(input$redraw_graph)

  # data that is filtered between the start and end time inputs, used for drawing graphs,
  # aliquot and EMC calculations
  filtered_data <- reactive({
    flow <- data()$flow
    sample <- data()$sample
    joined <- data()$joined

    flow_filtered <- flow |>
      filter(between(mins, input$lower_mins, input$upper_mins))

    sample_filtered <-  sample |>
      filter(between(mins, input$lower_mins, input$upper_mins))

    joined_filtered <-  joined |>
      filter(between(mins, input$lower_mins, input$upper_mins))

    proportions <- calculate_bottle_proportions(flow_filtered, joined_filtered, flow_units()$time_unit, input$composite_vol)

    list(flow = flow_filtered, sample = sample_filtered, joined = joined_filtered, proportions = proportions)
  }) |>
    bindEvent(data(), input$redraw_graph)


  ######## Show actual datetimes below start and end minute inputs
  output$start_time <- renderText({
    format(input$lower_mins*60 + data()$flow$times[1], "%Y-%m-%d %H:%M:%S")
  }) |>
    bindEvent(input$lower_mins, input$submit)

  output$end_time <- renderText({
    format(input$upper_mins*60 + data()$flow$times[1], "%Y-%m-%d %H:%M:%S")
  }) |>
    bindEvent(input$upper_mins, input$submit)

  ########


  ######## check if concentration/pollutant data is included
  # data types/other checks done with file_validator, so only need to check number of columns in sheet
  has_conc <- reactive({
    ncol(data()$sample) > 2
  })

  ########

  ######## record flow units

  flow_units <- reactive({
    vol_unit = strsplit(input$flow_units, "/")[[1]][1]
    time_unit = strsplit(input$flow_units, "/")[[1]][2]
    list(vol_unit = vol_unit, time_unit = time_unit)
  })


  ########


  ######## tab control based on whether pollutant data is included, updates dynamically
  tabs_list <- reactiveValues(data = list("Flow-Weighting" = NULL, "Event Mean Concentration" = NULL))

  observe({
    if (is.null(tabs_list[['Flow-Weighting']]) & file_validator$is_valid()) {
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
              downloadLink("download_aliquot", "Download Aliquot Volume Table"),
              br(),
              DT::dataTableOutput("proportions", width = "100%")
            ),
            column(
              8,
              align = "center",
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
      tabs_list[["Flow-Weighting"]] = "Flow-Weighting"
    }
  }) |>
    bindEvent(input$submit)

  observe({
    if (is.null(tabs_list[['Event Mean Concentration']]) & has_conc() & file_validator$is_valid()) {
      insertTab(
        "full_page",
        tab = tabPanel(
          title = "Event Mean Concentration",
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
              align = "center",
              tableOutput("EMC_table"),
              downloadLink("download_pollutograph", label = "Download Pollutograph(s)"),
              br(),
              plotOutput("EMCgraph", height = 500)
            )
          )
        )
      )
      tabs_list[['Event Mean Concentration']] = "Event Mean Concentration"
    }
  }) |>
    bindEvent(input$submit)

  observe({
    if (!has_conc()) {
      updateTabsetPanel(session = session, inputId = "full_page", selected = "Flow-Weighting")
      tabs_list[["Event Mean Concentration"]] <- NULL
      removeTab("full_page", "Event Mean Concentration")
    }
  }) |>
    bindEvent(input$submit)

  observe({
    updateTabsetPanel(session = session, inputId = "full_page", selected = "Instructions")
    tabs_list[["Event Mean Concentration"]] <- NULL
    tabs_list[["Flow-Weighting"]] <- NULL
    removeTab("full_page", "Event Mean Concentration")
    removeTab("full_page", "Flow-Weighting")
  }) |>
    bindEvent(req(!file_validator$is_valid()))

  ########

  ######## generate graphs from data
  hydrograph <- reactive({
    flow <- data()$flow
    sample <- data()$sample
    joined <- data()$joined

    ggplot() +
      geom_line(data = flow, aes(x = mins, y = flow_values, color = 'Flow'), linewidth = 1.5) +
      geom_point(data = joined, aes(x = mins, y = values, color = 'Sample Collected'), size = 3) +
      coord_cartesian(xlim = c(xmin, xmax()), ylim = c(ymin, ymax()), expand = FALSE) +
      scale_x_continuous(breaks = scales::breaks_extended(n = 20)) +
      labs(x = 'Time since start (min)', y = paste0('Flow ', '(', input$flow_units, ')'), color = NULL, title = input$title) +
      theme(
        text = element_text(size = global_font_size),
        legend.position = 'top',
        legend.justification = c("center", "top"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 0.5)
      ) +
      scale_color_manual(values = c("#f8766d", "#357bb7")) +
      guides(
        color = guide_legend(
          override.aes = list(shape = c(NA, 16), linetype = c(1, NA))
        )
      )
  })

  # render hydrograph and add annotations/highlights
  output$hydrograph <- renderPlot({
    hydrograph() +
      annotate("rect", xmin = xmin, xmax = ifelse(is.na(input$lower_mins), xmin, input$lower_mins),
               ymin = ymin, ymax = ymax(), alpha = 0.2) +
      annotate("rect", xmin = ifelse(is.na(input$upper_mins), xmax(), input$upper_mins), xmax = xmax(),
               ymin = ymin, ymax = ymax(), alpha = 0.2)
  })|>
    bindEvent(input$redraw_graph)

  # may need multiple pollutographs, so use function to plot the data
  EMCgraph <- reactive({
    flow <- data()$flow
    sample <- data()$sample

    plot_pollutograph <- function(sample, flow) {
      # need a scale factor for second y axis to be scaled correctly
      scale_factor <- max(sample$conc_values)/max(flow$flow_values)

      ggplot() +
        geom_line(data = flow, aes(x = mins, y = flow_values, color = 'Flow'), linewidth = 1.5) +
        geom_point(data = sample, aes(x = mins, y = conc_values/scale_factor,
                                      color = 'Sample Concentration'), shape = 17, size = 3) +
        coord_cartesian(xlim = c(0, xmax()), ylim = c(ymin, ymax()), expand = FALSE) +
        scale_x_continuous(breaks = scales::breaks_extended(n = 20)) +
        scale_y_continuous(sec.axis = sec_axis(~ . * scale_factor, name = sample$conc[1])) +
        labs(title = input$title, x = 'Time since start (min)', y = paste0('Flow ', '(', input$flow_units, ')'), color = NULL) +
        theme(
          text = element_text(size = global_font_size),
          legend.position = 'bottom',
          legend.justification = c("center", "top"),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = 0.5)
        ) +
        scale_color_manual(values = c("#f8766d", "#357bb7")) +
        guides(
          color = guide_legend(
            override.aes = list(shape = c(NA, 17), linetype = c(1, NA))
          )
        )
    }

    sample_list <- split(sample, sample$conc)
    plot_list <- lapply(sample_list, plot_pollutograph, flow = flow)
    plot_list
  })

  # render list of plots created with pollutograph function and add annotations/highlights
  output$EMCgraph <- renderPlot({
    plot_list_out <- lapply(EMCgraph(), function(p) p +
                              annotate("rect", xmin = xmin, xmax = input$lower_mins,
                                       ymin = ymin, ymax = ymax(), alpha = 0.2) +
                              annotate("rect", xmin = input$upper_mins, xmax = xmax(),
                                       ymin = ymin, ymax = ymax(), alpha = 0.2))

    # if pollutographs fail to generate properly (in between numeric inputs for filtering for example)
    # then print an empty plot rather than error message
    tryCatch(cowplot::plot_grid(plotlist=plot_list_out, ncol = ifelse(length(plot_list_out) == 1, 1, 2)),
             error = function(cond) ggplot())

  }, height = function() (ceiling((dim(data()$sample)[2] - 2)/2))*500) |>
    bindEvent(input$redraw_graph)

  ########

  ######## table generation
  proportions <- reactive({
    filtered <- filtered_data()

    prop_out <- tryCatch({
      filtered$proportions |>
        select(SampleTime, AliquotVolume) |>
        tibble::add_column(`Minutes Since Start` = filtered$joined$mins) |>
        rename(`Sample Times` = SampleTime,
               `Aliquot Volume (mL)` = AliquotVolume) |>
        mutate(`Sample Times` = paste(`Sample Times`),
               `Aliquot Volume (mL)` = round(`Aliquot Volume (mL)`, 1)) |>
        relocate(`Minutes Since Start`, .after = `Sample Times`)
    }, error = function(cond) return(tibble(`Sample Times` = NA, `Minutes Since Start` = NA, `Aliquot Volume (mL)` = NA)))

    summary_row <- prop_out |>
      summarize(`Sample Times` = "Total", `Minutes Since Start` = NA, `Aliquot Volume (mL)` = sum(`Aliquot Volume (mL)`))

    prop_out <- prop_out |>
      add_row(summary_row)
    prop_out
  })

  # render proportions table to page
  output$proportions <- DT::renderDataTable({
    proportions()
  }, options = list(searching = FALSE, columnDefs = list(list(width = '155px', targets = 1))), selection = 'none') |>
    bindEvent(input$redraw_graph)

  # generate filtered concentration/pollutant  sample table
  # sample data is in long format, so pivot it to wide format
  concentrations <- reactive({
    sample <- filtered_data()$sample

    sample |>
      rename(`Sample Times` = times,
             `Minutes Since Start` = mins) |>
      mutate(`Sample Times` = paste(`Sample Times`)) |>
      relocate(`Minutes Since Start`, .after = `Sample Times`) |>
      tidyr::pivot_wider(names_from = conc, values_from = conc_values)
  })

  # generate as datatable with searching and row selection options removed
  output$conc_table <- DT::renderDataTable({
    concentrations()
  }, options = list(searching = FALSE, autoWidth = TRUE, columnDefs = list(list(width = '155px', targets = 1))), selection = 'none') |>
    bindEvent(input$redraw_graph)

  # table to show EMC values, one per pollutant
  EMC <- reactive({
    props <- filtered_data()$proportions
    sample <- filtered_data()$sample

    sample |>
      group_by(conc) |>
      summarize(across(.cols = !(c(mins, times)), .fns = ~ signif(as.numeric(props$Proportions%*%.x), 3))) |>
      rename(`Pollutant` = conc, `Event Mean Concentration` = conc_values)
  })

  # render table to page
  output$EMC_table <- renderTable({
    EMC()
  }, align = "l", striped = TRUE, display = c("d", "s", "fg")) |>
    bindEvent(input$redraw_graph)

  ########

  ######## volume text output
  # can't output same thing in two html divs, so save same output to 2 vars
  output$volume1 <- output$volume2 <- renderText({
    flow <- filtered_data()$flow
    sample <- filtered_data()$sample
    joined <- filtered_data()$joined

    vol_out <- round(sum(calculate_bottle_proportions(flow, joined, flow_units()$time_unit)$Volume), 1)
    paste('Total Hydrograph Volume:', vol_out, flow_units()$vol_unit)
  }) |>
    bindEvent(input$redraw_graph)
  ########

  ######## download handlers for graphs, tables, and data template
  # shiny currently has a bug where downloads don't work immediately after app loads
  # (downloads a 'download.htm' file instead)
  # this happens with the datatemplate if user tries to download the template
  # until about 30 seconds after loading
  output$download_template <- downloadHandler(
    filename = "Flow-Weighting_Template.xlsx",
    content = function(file) {
      template_path <- "inst/extdata/DataTemplate.xlsx"
      file.copy(template_path, file, overwrite = TRUE)
    }
  )

  # graphs are 4:3 aspect ratio
  output$download_hydrograph <- downloadHandler(
    filename = function() {
      paste0("Hydrograph-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".png")
    },
    content = function(file) {
      cowplot::save_plot(file, plot = hydrograph(), device = "png", base_height = 6.94, base_asp = 1.33)
    }
  )

  # number of rows dependent on number of generated plots
  output$download_pollutograph <- downloadHandler(
    filename = function() {
      paste0("Pollutograph-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".png")
    },
    content = function(file) {
      num_cols <- ifelse(length(EMCgraph()) == 1, 1, 2)
      plot <- cowplot::plot_grid(plotlist=EMCgraph(), ncol = num_cols) + theme(plot.background = element_rect(fill = "white", color = NA))
      cowplot::save_plot(file, plot = plot, device = "png", ncol = num_cols, nrow = ceiling(length(EMCgraph())/2), base_height = 6.94, base_asp = 1.33)
    }
  )

  output$download_aliquot <- downloadHandler(
    filename = function() {
      paste0("AliquotVolume-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      utils::write.csv(proportions(), file, row.names = FALSE)
    }
  )
  ########
}
