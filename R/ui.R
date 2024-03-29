markdown_text <- httr::GET("https://raw.githubusercontent.com/SCCWRP/FWCCalculator/main/README.md") |>
  httr::content()

ui <- fluidPage(
  shinyjs::useShinyjs(),

  tags$head(tags$style(paste0('body {font-size: ', global_font_size, 'px}
                              .action-button {font-size: ', global_font_size, 'px}
                              #volume1 {font-weight: bold}
                              #volume2 {font-weight: bold}
                              #start_time {color: Gray}
                              #end_time {color: Gray}')),
            tags$style(
              ".wrap-button-text .btn { white-space: normal; }"
            )),

  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),

  titlePanel("Flow-Weighting & Compositing Calculator"),
  fluidRow(
    column(
      3,
      p("This web application has been developed to enable consistent, transparent, easily applied calculations for post-storm flow-weighting and compositing and/or to generate an Event Mean Concentration (EMC) from a pollutograph. The web app provides flow-weighted compositing instructions based on a user-uploaded hydrograph and times of sample collection, or returns an EMC based on a user-uploaded hydrograph and pollutograph. Total hydrograph volume is also returned so that users may determine a mass load from the EMC.")#,
      #h4(strong("Known Issues")),
      # p("This is a beta release of the application. The following are known issues and will be addressed in the next beta release of the application."),
      # HTML('
      #   <ul>
      #     <li>
      #       In the case where sample data have one less row than the flow data and start at the same time as the flow data, no output is generated.
      #     </li>
      #   </ul>
      # ')
    ),
    column(
      3,
      align = "left",
      strong("Step 1: Download Template"),
      br(),
      column(
        12,
        "Overwrite the template with your data. See Data Requirements section below.",
        br(),
        downloadButton("download_template1"),
        br(),
        br()
      ),
      strong("Step 2: Submit Data"),
      column(
        9,
        div(
          fileInput(
            "file",
            "Choose Excel File",
            multiple = FALSE,
            accept = ".xlsx"
          ),
          style = 'overflow-y:auto; height: 300px;'
        )
      ),
      column(
        3,
        br(),
        shinyjs::disabled(
          actionButton("submit", "Submit")
        )
      )
    ),
    column(
      3,
      align = "left",
      fluidRow(
        strong("Step 3: Select Input Filter Parameters"),
        column(
          12,
          "Optional: The Start and End Date/Time inputs can be used to reduce the calculations to the time period of interest. Start and End Dates/Times outside the time range of the submitted data are reset to the beginning and end of the submitted data, respectively. The Composite Volume input controls the total volume in the Aliquot Volume calculation, with a minimum of 500 mL and maximum of 10,000 mL."
        )
      ),
      fluidRow(
        column(
          4,
          shinyjs::disabled(
            tagAppendAttributes(
              dateInput(
                "start_date",
                label = "Start Date",
                autoclose = TRUE
              ),
              onkeydown ='return false'
            )
          )
        ),
        column(
          8,
          shinyjs::disabled(
            shinyTime::timeInput(
              "start_time",
              label = div("Start Time", style="color:black"),
              value = lubridate::hm("12:00"),
              seconds = FALSE
            )
          )
        )
      ),
      fluidRow(
        column(
          4,
          shinyjs::disabled(
            tagAppendAttributes(
              dateInput(
                "end_date",
                label = "End Date",
                autoclose = TRUE
              ),
              onkeydown = 'return false'
            )
          )
        ),
        column(
          8,
          shinyjs::disabled(
            shinyTime::timeInput(
              "end_time",
              label = div("End Time", style="color:black"),
              value = lubridate::hm("12:00"),
              seconds = FALSE
            )
          )
        )
      ),
      fluidRow(
        column(
          4,
          shinyjs::disabled(
            numericInput(
              "composite_vol",
              "Composite Vol. (mL)",
              value = 1000,
              min = 500,
              max = 10000,
              width = "200px"
            )
          )
        ),
        column(
          8,
          selectInput(
            "flow_units",
            "Flow Units of Submitted Data",
            c(`L/s` = "L/s", `gal/min (gpm)` = "gal/min", `ft³/s (cfs)` = "ft³/s"),
            selected = "L/s",
            width = "200px"
          )
        )
      )
    ),
    column(
      2,
      align = "left",
      fluidRow(
        strong("Step 4: Draw Graph(s)"),
        br(),
        column(
          12,
          textInput(
            inputId = "title",
            label = "Graph Title",
            placeholder = "Enter an optional title for the graph(s)",
            value = "",
            width = "100%"
          ),
          shinyjs::disabled(
            actionButton("redraw_graph", "Draw Graph(s)", width = "175px")
          )
        )
      ),
      br(),
      br(),
      br(),
      strong("Submit New Data"),
      br(),
      actionButton("reset_button", "Reload App", width = "175px"),
      br(),
      br(),
      shinyWidgets::actionBttn(
        "feedback",
        "Feedback Form",
        width = '250px',
        onclick ="window.open('https://forms.office.com/pages/responsepage.aspx?id=PfKopOEaHEuZAuqhUwKBkNb1vpfauiZNit2g-l_MjnRUNVJWVFlFRzdLOVVPODlYMllLNjE3RU44Vy4u&web=1&wdLOR=cBEFC20B7-3BF7-4F6B-ADE9-65CD584DA1A3', '_blank')"
      ),
      br(),
      br(),
      shinyWidgets::actionBttn(
        "apidoc",
        HTML("Access the code & <br> documentation on GitHub"),
        width = '250px',
        onclick ="window.open('https://github.com/SCCWRP/FWCCalculator', '_blank')"
      )
    )
  ),
  tabsetPanel(
    tabPanel(
      "Instructions",
      h3("Using this Calculator"),
      "This calculator will produce a table of aliquot volume values, a hydrograph, and, if pollutant data is provided, pollutograph(s) for the given data set of flow rate measurements and sample timestamps.",
      HTML("
        <ol>
          <li>
            Download the Excel template file ",), downloadLink("download_template2", label = "here"), HTML(" and overwrite it with your data. See the Data Requirements section below. <br><strong>NOTE</strong>: a 'download.htm' file may be downloaded instead of the template Excel file if the link is clicked too soon after launching the application. This is a known issue with the 'shiny' R package which was used to develop this application. Please allow a few minutes before downloading the template.
          </li>
          <li>
            Upload your data by clicking the 'Browse' button, selecting the updated Excel spreadsheet, and clicking the 'Submit' button. The calculator will generate the aliquot volume table as well as the hydrograph and pollutograph(s), depending on the uploaded data. If pollutant data is provided, the calculator will also provide the Event Mean Concentration for each of the specified pollutants.
          </li>
          <li>
            Use the 'Start Date/Time' and 'End Date/Time' inputs to filter the data to the appropriate time range. The grayed-out sections of the graph will <i>not</i> be included in the aliquot volume and event mean concentration calculations.
          </li>
          <li>
            After changing the 'Start Date/Time' and 'End Date/Time' inputs, click the 'Draw Graph(s)' button to regenerate the aliquot volume table, hydrograph, and pollutograph(s), filtered to the provided times.
          </li>
          <li>
            The 'Composite Vol.' input is used in the aliquot volume calculation such that the sum of the aliquot volumes will be equal to the composite volume value entered here, measured in mL. The minimum and maximum supported values are 500 mL and 10,000 mL, respectively.
          </li>
          <li>
            The 'Flow Units of Submitted Data' input is used to label and calculate the 'Total Hydrograph Volume' output.
          </li>
          <li>
            Use the 'Reload App' button to submit a new data set.
          </li>
        </ol>
           "),
      hr(),
      h3("Data Requirements"),
      HTML("The uploaded Excel spreadsheet must conform to the following requirements:
        <ul>
          <li>
            Must contain exactly three sheets, in the following order:
          </li>
          <ul>
            <li>
              Instructions: instructions for using the calculator
            </li>
            <li>
              Sheet 1: flow rate measurement data
            </li>
            <li>
              Sheet 2: sample collection timestamps and pollutant measurement data (where applicable)
            </li>
          </ul>
          <li>
            The flow rate measurement data sheet (Sheet 1) must have exactly two columns:
          </li>
          <ul>
            <li>
              Col 1: timestamps in 'mm/dd/yy hh:mm:ss' format. Date and time must be provided.
            </li>
            <ul>
              <li>
                The 'Datetime' column in the provided template file is already in the correct format.
              </li>
            </ul>
            <li>
              Col 2: flow rate measurements. <strong>Flow rates must be entered as L/s, gpm, or cfs.</strong>
            </li>
          </ul>
          <li>
            The sample collection timestamps and pollutant measurement sheet (Sheet 2) may have any number of columns:
          </li>
          <ul>
            <li>
              Col 1: timestamps when water quality samples were collected in 'mm/dd/yy hh:mm:ss' format. Date and time must be provided.
            </li>
            <ul>
              <li>
                The 'Datetime' column in the provided template file is already in the correct format.
              </li>
            </ul>
            <li>
              Col 2...n: pollutant concentrations, if/where available
            </li>
            <li>
              Any number of pollutant columns in the second sheet are supported.
            </li>
            <li>
              <strong>If you do not have pollutant data, delete the 'Pollutant' columns entirely before uploading the template. Do not delete Sheet 2.</strong>
            </li>
          </ul>
          <li>
            The column headers are required and can be renamed as needed, but cannot be exclusively numeric characters [0-9].
          </li>
          <li>
            All flow rate and pollutant measurements must be greater than zero.
          </li>
          <li>
            There may not be any missing values in the spreadsheet.
          </li>
        </ul>")
    ),
    tabPanel(
      "Methods",
      markdown_text |>
        commonmark::markdown_html() |>
        HTML() |>
        withMathJax()
    ),
    tabPanel(
      "Contact Us",
      br(),
      HTML(
        '
        If you have any additional questions, please contact us at
          <a href="mailto:stormwater@sccwrp.org?subject=Flow-Weighting and Compositing Calculator Question">stormwater@sccwrp.org</a>.
        '
      ),
      br(),
      HTML(
        '
        <h3>Frequently Asked Questions (FAQs)</h3>

        <p>Question: Can I upload multiple storms for batch processing?
        <br>Answer: No, the present version of the calculator only supports single event processing.
        </p>


        <p>Question: Can I upload multiple pollutants?
        <br>Answer: Yes, but every pollutant must have a concentration value in every row with a timestamp on Sheet 2. For example, the following Sheet 2 upload will return a Missing Values error message for the Total Copper column.
        </p>
        '
      ),
      imageOutput("missing_val", height="240px"),
      br(),
      HTML(
        '
        <p>Question: How do I represent intermediate composites? For example, I\'ve sampled for TSS and Total Copper, but I combined every other TSS sample to reduce overall lab costs.
        <br>Answer: See the following example. The first submission will return an error message, but the second will pass.
        </p>
        '
      ),
      imageOutput("merged_cells", height="201px"),
      br(),
      imageOutput("repeated_cells", height="201px"),
      br(),
      HTML(
        '
        <p>Question: How do I know if I have paired or unpaired data?
        <br>Answer: Data is paired if the array of timestamps for flowrate is identical to the array of timestamps for concentration. In other words, for every flowrate observation there is a concentration value and vis versa.
        </p>
        '
      )
    ),
    id = "full_page"
  )
)
