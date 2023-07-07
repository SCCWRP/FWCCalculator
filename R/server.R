server <- function(input, output, session) {

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

  file_validator$add_rule("file", function(file) has_three_sheets(file))

  file_validator$add_rule("file", function(file) has_two_columns(file))

  file_validator$add_rule("file", function(file, sheet) has_headers(file, sheet), sheet = 2)
  file_validator$add_rule("file", function(file, sheet) has_headers(file, sheet), sheet = 3)

  file_validator$add_rule("file", function(file, sheet) has_correct_date_format(file, sheet), sheet = 2)
  file_validator$add_rule("file", function(file, sheet) has_correct_date_format(file, sheet), sheet = 3)

  file_validator$add_rule("file", function(file) has_sample_times_in_range(file))

  file_validator$add_rule("file", function(file, sheet) has_correct_measurement_format(file, sheet), sheet = 2)
  file_validator$add_rule("file", function(file, sheet) has_correct_measurement_format(file, sheet), sheet = 3)

  file_validator$add_rule("file", function(file, sheet) has_no_missing_values(file, sheet), sheet = 2)
  file_validator$add_rule("file", function(file, sheet) has_no_missing_values(file, sheet), sheet = 3)

  file_validator$add_rule("file", function(file) has_no_negative_values(file))

  observe({
    file_validator$enable()
    shinyjs::toggleState("start_date", file_validator$is_valid())
    shinyjs::toggleState("start_time", file_validator$is_valid())
    shinyjs::toggleState("end_date", file_validator$is_valid())
    shinyjs::toggleState("end_time", file_validator$is_valid())
    shinyjs::toggleState("composite_vol", file_validator$is_valid())
    shinyjs::toggleState("flow_units", file_validator$is_valid())
    shinyjs::toggleState("redraw_graph", file_validator$is_valid())
    shinyjs::toggleState("submit", file_validator$is_valid())
  }) |>
    bindEvent(req(input$file))

  ########



  ######## Initial data input from excel file, set some global values related to data()
  data <- reactive({
    flow <- readxl::read_excel(input$file$datapath, sheet = 2)
    sample <- readxl::read_excel(input$file$datapath, sheet = 3)

    data_out <- clean_data(flow, sample)

    datetime_min <- lubridate::force_tz(min(data_out$flow$times), global_time_zone)
    datetime_max <- lubridate::force_tz(max(data_out$flow$times), global_time_zone)

    updateDateInput(
      session = session,
      inputId = "start_date",
      value = lubridate::date(datetime_min),
      min = lubridate::date(datetime_min),
      max = lubridate::date(datetime_max)
    )

    shinyTime::updateTimeInput(
      session = session,
      inputId = "start_time",
      value = datetime_min
    )

    updateDateInput(
      session = session,
      inputId = "end_date",
      value = lubridate::date(datetime_max),
      min = lubridate::date(datetime_min),
      max = lubridate::date(datetime_max)
    )

    shinyTime::updateTimeInput(
      session = session,
      inputId = "end_time",
      value = datetime_max
    )


    data_out
  }) |>
    bindEvent(input$submit)

  input_start_utc <- reactive({
    req(input$start_date)
    input_start_utc <- input$start_time
    lubridate::date(input_start_utc) <- input$start_date

    lubridate::force_tz(input_start_utc, global_time_zone) |>
      as.POSIXct(tz = global_time_zone)

  })

  input_end_utc <- reactive({
    req(input$end_date)
    input_end_utc <- input$end_time
    lubridate::date(input_end_utc) <- input$end_date

    lubridate::force_tz(input_end_utc, global_time_zone) |>
      as.POSIXct(tz = global_time_zone)
  })



  lower_mins <- reactive({
    req(input$start_date)
    data_start <- date_min()

    start_time <- input$start_time

    lubridate::date(start_time) <- input$start_date

    start_time <- lubridate::force_tz(start_time, global_time_zone)

    lubridate::time_length(start_time - data_start, unit = "mins")
  }) |>
    bindEvent(input$redraw_graph, input$submit)


  upper_mins <- reactive({
    req(input$end_date)
    data_start <- date_min()

    end_time <- input$end_time
    lubridate::date(end_time) <- input$end_date

    end_time <- lubridate::force_tz(end_time, global_time_zone)
    lubridate::time_length(end_time - data_start, unit = "mins")

  }) |>
    bindEvent(input$redraw_graph, input$submit)


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

  date_min <- reactive({
    lubridate::force_tz(min(data()$flow$times), global_time_zone)
  })

  date_max <- reactive({
    lubridate::force_tz(max(data()$flow$times), global_time_zone)
  })


  ########


  # update numeric inputs on invalid values
  observe({
    if (lower_mins() < xmin | is.na(lower_mins())) {
      shinyTime::updateTimeInput(session = session, inputId = "start_time", value = date_min())
      updateDateInput(session = session, inputId = "start_date", value = lubridate::date(date_min()))
    }
    if (upper_mins() > xmax() | is.na(upper_mins())) {
      shinyTime::updateTimeInput(session = session, inputId = "end_time", value = date_max())
      updateDateInput(session = session, inputId = "end_date", value = lubridate::date(date_max()))
    }
    if (lower_mins() > xmax()) {
      shinyTime::updateTimeInput(session = session, inputId = "start_time", value = date_max() - lubridate::minutes(30))
      updateDateInput(session = session, inputId = "start_date", value = lubridate::date(date_max() - lubridate::minutes(30)))

      shinyTime::updateTimeInput(session = session, inputId = "end_time", value = date_max())
      updateDateInput(session = session, inputId = "end_date", value = lubridate::date(date_max()))

    }
    if (lower_mins() > upper_mins()) {
      shinyTime::updateTimeInput(session = session, inputId = "end_time", value = ifelse(lower_mins() + 30 <= xmax(),  date_min() + lubridate::minutes(30), date_max()))
      updateDateInput(session = session, inputId = "end_date", value = lubridate::date(ifelse(lower_mins() + 30 <= xmax(),  date_min() + lubridate::minutes(30), date_max())))

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
      filter(between(mins, lower_mins(), upper_mins()))

    sample_filtered <-  sample |>
      filter(between(mins, lower_mins(), upper_mins()))

    joined_filtered <-  joined |>
      filter(between(mins, lower_mins(), upper_mins()))

    proportions <- calculate_bottle_proportions(flow_filtered, joined_filtered, flow_units()$time_unit, input$composite_vol)

    list(flow = flow_filtered, sample = sample_filtered, joined = joined_filtered, proportions = proportions)
  }) |>
    bindEvent(data(), input$redraw_graph)


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
        target = "Methods",
        tab = tabPanel(
          "Flow-Weighting",
          fluidRow(
            column(
              4,
              align = "center",
              br(),
              textOutput("volume1"),
              shinyjs::hidden(downloadLink("download_results", "Download Results")),
              br(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("proportions", width = "100%")
              )
            ),
            column(
              8,
              align = "center",
              br(),
              br(),
              shinyjs::hidden(downloadLink("download_hydrograph", label = "Download Hydrograph")),
              br(),
              shinycssloaders::withSpinner(
                plotOutput("hydrograph", height = "500px")
              )
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
        target = "Flow-Weighting",
        tab = tabPanel(
          title = "Event Mean Concentration",
          fluidRow(
            column(
              4,
              align = "center",
              br(),
              textOutput("volume2"),
              shinyjs::hidden(downloadLink("download_results2", "Download Results")),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("conc_table", width = "100%")
              )
            ),
            column(
              3,
              alignt = "center",
              br(),
              br(),
              br(),
              br(),
              br(),
              div(
                shinycssloaders::withSpinner(
                  tableOutput("EMC_table")
                ),
                style = "overflow-y:auto"
              )
            ),
            column(
              5,
              align = "center",
              br(),
              br(),
              shinyjs::hidden(downloadLink("download_pollutograph", label = "Download Pollutograph(s)")),
              br(),
              br(),
              br(),
              div(
                shinycssloaders::withSpinner(
                  plotOutput("EMCgraph", width = "95%")
                ),
                style = "height:475px; overflow-y:auto"
              )
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
      geom_line(data = flow, aes(x = times, y = flow_values, color = 'Flow'), linewidth = 1.5) +
      geom_point(data = joined, aes(x = times, y = values, color = 'Sample Collected'), size = 3) +
      scale_x_datetime(
        breaks = scales::breaks_pretty(n = 20),
        labels = scales::label_date(format = "%m-%d %H:%M"),
        expand = expansion()
      ) +
      labs(x = 'Date & Time', y = paste0('Flow ', '(', input$flow_units, ')'), color = NULL, title = input$title) +
      theme(
        text = element_text(size = global_font_size),
        legend.position = 'top',
        legend.justification = c("center", "top"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)
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

    plot <- hydrograph() +
      annotate(
        "rect",
        xmin = date_min(),
        xmax = input_start_utc(),
        ymin = ymin,
        ymax = ymax(),
        alpha = 0.2
      ) +
      annotate(
        "rect",
        xmin = input_end_utc(),
        xmax = date_max(),
        ymin = ymin,
        ymax = ymax(),
        alpha = 0.2
      )
    shinyjs::show("download_hydrograph")
    plot

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
        geom_line(data = flow, aes(x = times, y = flow_values, color = 'Flow'), linewidth = 1.5) +
        geom_point(data = sample, aes(x = times, y = conc_values/scale_factor,
                                      color = 'Sample Concentration'), shape = 17, size = 3) +
        scale_x_datetime(breaks = scales::breaks_pretty(n = 20), labels = scales::label_date(format = "%m-%d %H:%M"), expand = expansion()) +
        scale_y_continuous(sec.axis = sec_axis(~ . * scale_factor, name = sample$conc[1])) +
        labs(
          title = input$title,
          x = 'Date & Time',
          y = glue::glue('Flow ({input$flow_units})'),
          color = NULL
        ) +
        theme(
          text = element_text(size = global_font_size),
          legend.position = 'bottom',
          legend.justification = c("center", "top"),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5)
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
                              annotate("rect", xmin = date_min(), xmax = input_start_utc(),
                                       ymin = ymin, ymax = ymax(), alpha = 0.2) +
                              annotate("rect", xmin = input_end_utc(), xmax = date_max(),
                                       ymin = ymin, ymax = ymax(), alpha = 0.2))
    shinyjs::show("download_pollutograph")
    tryCatch(cowplot::plot_grid(plotlist=plot_list_out, ncol = 1),
             error = function(cond) ggplot())


  }, height = function() (length(unique((data()$sample$conc))))*450) |>
    bindEvent(input$redraw_graph)

  ########

  ######## table generation
  proportions <- reactive({
    filtered <- filtered_data()

    prop_out <- tryCatch({
      filtered$proportions |>
        select(SampleTime, AliquotVolume) |>
        rename(`Sample Times` = SampleTime,
               `Aliquot Volume (mL)` = AliquotVolume) |>
        mutate(`Sample Times` = paste(`Sample Times`),
               `Aliquot Volume (mL)` = round(`Aliquot Volume (mL)`, 1))
    }, error = function(cond) return(tibble(`Sample Times` = NA, `Aliquot Volume (mL)` = NA)))

    summary_row <- prop_out |>
      summarize(`Sample Times` = "Total", `Aliquot Volume (mL)` = sum(`Aliquot Volume (mL)`))

    prop_out <- prop_out |>
      add_row(summary_row)

    prop_out
  })

  # render proportions table to page
  output$proportions <- DT::renderDataTable({
    props <- proportions()
    shinyjs::show("download_results")
    props
  }, options = list(searching = FALSE, lengthChange = FALSE, columnDefs = list(list(width = '155px', targets = 1))), selection = 'none') |>
    bindEvent(input$redraw_graph)

  # generate filtered concentration/pollutant  sample table
  # sample data is in long format, so pivot it to wide format
  concentrations <- reactive({
    sample <- filtered_data()$sample

    sample |>
      select(-mins) |>
      rename(`Sample Times` = times) |>
      mutate(`Sample Times` = paste(`Sample Times`)) |>
      tidyr::pivot_wider(names_from = conc, values_from = conc_values)
  })

  # generate as datatable with searching and row selection options removed
  output$conc_table <- DT::renderDataTable({
    concentrations()
  },
  extensions = "FixedColumns",
  options = list(
    fixedColumns = list(leftColumns = 2),
    scrollX = TRUE,
    lengthChange = FALSE,
    searching = FALSE,
    autoWidth = TRUE,
    columnDefs = list(list(width = '155px', targets = 1))
  ),
  selection = 'none') |>
    bindEvent(input$redraw_graph)

  # table to show EMC values, one per pollutant
  EMC <- reactive({
    props <- filtered_data()$proportions
    sample <- filtered_data()$sample
    flow <- filtered_data()$flow

    num_concs <- sample |>
      distinct(conc) |>
      pull() |>
      length()


    if(nrow(flow) == (nrow(sample) / num_concs)) {
      emc_out <- sample |>
        group_by(conc) |>
        slice_head(n=-1) |>
        summarize(conc_values = signif(as.numeric(props$Proportions%*%conc_values), 3)) |>
        rename(`Pollutant` = conc, `Event Mean Concentration` = conc_values)
      return(emc_out)
    }

    emc_out <- sample |>
      group_by(conc) |>
      summarize(conc_values = signif(as.numeric(props$Proportions%*%conc_values), 3)) |>
      rename(`Pollutant` = conc, `Event Mean Concentration` = conc_values)

    emc_out
  })

  # render table to page
  output$EMC_table <- renderTable({
    emc <- EMC()
    shinyjs::show("download_results2")
    emc
  }, striped = TRUE, display = c("d", "s", "fg")) |>
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

      hydro_plot <- hydrograph() +
        scale_x_datetime(
          breaks = scales::breaks_pretty(n = 20),
          labels = scales::label_date(format = "%m-%d %H:%M"),
          limits = c(input_start_utc(), input_end_utc()),
          expand = expansion()
        ) +
        ylim(ymin, ymax())

      cowplot::save_plot(file, plot = hydro_plot, device = "png", base_height = 6.94, base_asp = 1.33)
    }
  )

  # number of rows dependent on number of generated plots
  output$download_pollutograph <- downloadHandler(
    filename = function() {
      paste0("Pollutograph-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".png")
    },
    content = function(file) {
      withProgress(
        message = "Downloading Pollutograph(s)",
        value = 0,
        {
          num_graphs <- length(EMCgraph())
          num_cols <- ifelse(num_graphs == 1, 1, 2)

          emc_list <- lapply(
            EMCgraph(), function(p) {
              incProgress(1/num_graphs)
              p +
                scale_x_datetime(
                  breaks = scales::breaks_pretty(n = 20),
                  labels = scales::label_date(format = "%m-%d %H:%M"),
                  limits = c(input_start_utc(), input_end_utc()),
                  expand = expansion()
                ) +
                ylim(ymin, ymax())
            }
          )

          plot <- cowplot::plot_grid(plotlist=emc_list, ncol = num_cols) + theme(plot.background = element_rect(fill = "white", color = NA))
          cowplot::save_plot(file, plot = plot, device = "png", ncol = num_cols, nrow = ceiling(length(EMCgraph())/2), base_height = 6.94, base_asp = 1.33)

        }
      )
    }
  )


  output$download_results <- output$download_results2 <- downloadHandler(
    filename = function() {
      glue::glue("Flow-Weighting-Results-{format(Sys.time(), '%Y-%m-%d-%H%M%S')}.xlsx")
    },
    content = function(file) {
      withProgress(
        message = "Downloading Results",
        value = 0,
        {
          flow <- filtered_data()$flow
          sample <- filtered_data()$sample
          joined <- filtered_data()$joined

          incProgress(1/10)
          vol_out <- round(sum(calculate_bottle_proportions(flow, joined, flow_units()$time_unit)$Volume), 1)

          incProgress(3/10)

          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Results")
          openxlsx::writeData(
            wb = wb,
            sheet = "Results",
            x = "Total Hydrograph Volume",
            colNames = FALSE
          )
          openxlsx::writeData(
            wb = wb,
            sheet = "Results",
            x = paste(vol_out, flow_units()$vol_unit),
            startCol = 2,
            colNames = FALSE
          )
          openxlsx::writeData(
            wb = wb,
            sheet = "Results",
            x = proportions(),
            startRow = 3
          )
          incProgress(5/10)

          if(!is.null(tabs_list[['Event Mean Concentration']])) {
            openxlsx::writeData(
              wb = wb,
              sheet = "Results",
              x = EMC(),
              startCol = 4,
              startRow = 3
            )
          }
          incProgress(1/10)
          openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE)
        }
      )
    }
  )
}
