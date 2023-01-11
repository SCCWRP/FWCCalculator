ui <- fluidPage(
  tags$head(tags$style(paste0('body {font-size: ', global_font_size, 'px}
                              .action-button {font-size: ', global_font_size, 'px}
                              #volume1 {font-weight: bold}
                              #volume2 {font-weight: bold}
                              #start_time {color: LightGray}
                              #end_time {color: LightGray}'))),

  titlePanel("Flow-Weighting and Compositing Calculator"),
  fluidRow(
    column(
      2,
      align = "left",
      fileInput(
        "file",
        "Choose Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      )
    ),
    column(
      4,
      align = "center",
      column(
        6,
        selectInput(
          "flow_choices",
          "Flow Units",
          c("", allowed_flow_units)
        )
      ),
      column(
        6,
        numericInput(
          "composite_vol",
          "Composite Vol. (mL)",
          value = 1000,
          min = 500,
          max = 10000
        )
      )
    ),
    column(
      4,
      align = "center",
      column(
        6,
        numericInput(
          "lower_mins",
          "Start Time",
          min = 0,
          value = 100,
          step = 5,
          width = "200px"
        ),
        textOutput("start_time")
      ),
      column(
        6,
        numericInput(
          "upper_mins",
          "End Time",
          min = 0,
          step = 5,
          value = 1000,
          width = "200px"
        ),
        textOutput("end_time")
      )
    ),
    column(
      2,
      align = "center",
      br(),
      actionButton("reset_button", "Reload Application")
    )
  ),
  tabsetPanel(
    tabPanel(
      "Instructions",
      br(),
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
      br(),
      br(),
      "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"
    ),
    id = "full_page"
  )
)
