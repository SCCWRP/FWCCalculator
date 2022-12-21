source("R/units.R", local = TRUE)

ui <- fluidPage(
  titlePanel("Flow-Weighting and Compositing Calculator"),
  conditionalPanel(
    "!input.submit",
    fileInput(
      "file",
      "Choose Excel File",
      multiple = FALSE,
      accept = ".xlsx"
    )
  ),
  fluidRow(
    column(
      3,
      selectInput(
        "flow_choices",
        "Flow Units",
        c("", allowed_units)
      ),
      conditionalPanel(
        "input.flow_choices != '' & output.fileUploaded & !input.submit",
        actionButton("submit", "Submit")
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
  conditionalPanel(
    "input.submit",
    tabsetPanel(
      tabPanel(
        "Flow-Weighting",
        fluidRow(
          column(
            3,
            conditionalPanel(
              "input.submit",
              downloadButton("download_data", "Download Results.csv"),
              tableOutput("proportions")
            )
          ),
          column(
            9,
            plotOutput("hydrograph", height = 500)
          )
        )
      ),
      id = "full_page"
    )
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
