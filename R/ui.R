ui <- fluidPage(
  tags$head(tags$style(paste0('body {font-size: ', global_font_size, 'px}
                              .action-button {font-size: ', global_font_size, 'px}
                              #volume1 {font-weight: bold; text-align: center}
                              #volume2 {font-weight: bold; text-align: center}'))),

  titlePanel("Flow-Weighting and Compositing Calculator"),
  fluidRow(
    column(
      12,
      conditionalPanel(
        "!input.submit",
        fileInput(
          "file",
          "Choose Excel File",
          multiple = FALSE,
          accept = ".xlsx"
        )
      ),
      conditionalPanel(
        "input.flow_choices != '' & output.fileUploaded & !input.submit",
        actionButton("submit", "Submit")
      )
    )
  ),
  fluidRow(
    column(
      3,
      selectInput(
        "flow_choices",
        "Flow Units",
        c("", allowed_flow_units)
      ),
      conditionalPanel(
        "output.has_conc",
        selectInput(
          "conc_choices",
          "Concentration Units",
          c("", allowed_concentration_units)
        )
      )
    ),
    column(
      3,
      numericInput(
        "composite_vol",
        "Composite Vol. (mL)",
        value = 1000,
        min = 500,
        max = 10000
      )
    ),
    column(
      3,
      conditionalPanel(
        "input.submit",
        sliderInput(
          "range",
          "Time range in minutes",
          min = 0,
          max = 1000,
          step = 5,
          value = c(100, 1000)
        )
      )
    ),
    column(
      3,
      conditionalPanel(
        "input.submit",
        actionButton("reset_button", "Upload New Data")
      )
    )
  ),
  tabsetPanel(
    tabPanel(
      "Instructions",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
      br(),
      br(),
      "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"
    ),
    id = "full_page"
  )
)


# ui <- navbarPage(
#   "Flow-Weighting and Compositing Calculator",
#   tabPanel(
#     "Flow-Weighting",
#     conditionalPanel(
#       "!input.submit",
#       fileInput(
#         "file",
#         "Choose Excel File",
#         multiple = FALSE,
#         accept = ".xlsx"
#       )
#     ),
#     fluidRow(
#       column(
#         3,
#         fluidRow(
#           column(
#             6,
#             selectInput(
#               "flow_choices",
#               "Flow Units",
#               c("", allowed_units)
#             ),
#             conditionalPanel(
#               "input.flow_choices != '' & output.fileUploaded & !input.submit",
#               actionButton("submit", "Submit")
#             )
#           ),
#           column(
#             6,
#             numericInput(
#               "composite_vol",
#               "Composite Vol. (mL)",
#               value = 1000,
#               min = 500,
#               max = 10000
#             )
#           )
#         ),
#         fluidRow(
#           column(
#             12,
#             conditionalPanel(
#               "input.submit",
#               downloadButton("download_data", "Download Results.csv"),
#               tableOutput("proportions")
#             )
#           )
#         )
#       ),
#       column(
#         9,
#         conditionalPanel(
#           "input.submit",
#           br(),
          # conditionalPanel(
          #   "input.submit",
          #   actionButton("reset_button", "Upload New Data")
          # ),
#           plotOutput("hydrograph", height = 500),
#           sliderInput(
#             "range",
#             "Time range in minutes",
#             min = 0,
#             max = 1000,
#             step = 5,
#             value = c(100, 1000)
#           )
#         )
#       )
#     )
#   ),
#   id = "full_page"
# )

# ui <- fluidPage(
#   titlePanel("Flow-Weighting and Compositing Calculator"),
#   sidebarLayout(
#     sidebarPanel(
#       conditionalPanel(
#         "!input.submit",
#         fileInput(
#           "file", "Choose Excel File",
#           multiple = FALSE,
#           accept = ".xlsx"
#         )
#       ),
#       fluidRow(
#         column(
#           6,
#           selectInput(
#             "flow_choices",
#             "Flow Units",
#             c("", allowed_units),
#             width = "150px"
#           )
#         ),
#         column(
#           6,
#           numericInput(
#             "composite_vol",
#             "Composite Volume (mL)",
#             value = 1000,
#             min = 500,
#             max = 10000,
#             width = "200px"
#           )
#         )
#       ),
#       conditionalPanel(
#         "input.flow_choices != '' & output.fileUploaded & !input.submit",
#         actionButton("submit", "Submit")
#       ),
#       conditionalPanel(
#         "input.submit",
#         downloadButton("download_data", "Download Results.csv"),
#         actionButton("reset_button", "Upload New Data"),
#         tableOutput("proportions")
#       )
#     ),
#     mainPanel(
#       conditionalPanel(
#         "input.submit",
#         tabsetPanel(
#           tabPanel(
#             "Flow-Weighting",
#             plotOutput("hydrograph", height = 600),
#             sliderInput(
#               "range",
#               "Time range in minutes",
#               min = 0,
#               max = 1000,
#               step = 5,
#               value = c(100, 1000)
#             )
#           ),
#           id = "tabs"
#         ),
#         textOutput("volume")
#       )
#     )
#   )
# )
