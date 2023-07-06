markdown_text <- httr::GET("https://raw.githubusercontent.com/SCCWRP/FWCCalculator/main/README.md") |>
  httr::content()

ui <- fluidPage(
  shinyjs::useShinyjs(),
  # HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
  #
  # tags$script('
  #   $(function() {
  #     var time_now = new Date()
  #     $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
  #   });
  # '),
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

  titlePanel("Flow-Weighting and Compositing Calculator"),
  fluidRow(
    column(
      4,
      "This web application has been developed to enable consistent, transparent, easily applied calculations for post-storm flow-weighting and compositing and/or to generate an Event Mean Concentration (EMC) from a pollutograph. The web app provides flow-weighted compositing instructions based on a user-uploaded hydrograph and times of sample collection, or returns an EMC based on a user-uploaded hydrograph and pollutograph. Total hydrograph volume is also returned so that users may determine a mass load from the EMC."
    ),
    column(
      2,
      align = "left",
      fileInput(
        "file",
        "Choose Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      shinyjs::disabled(
        numericInput(
          "composite_vol",
          "Composite Vol. (mL)",
          value = 1000,
          min = 500,
          max = 10000,
          width = "200px"
        )
      ),
      selectInput(
        "flow_units",
        "Flow Units of Submitted Data",
        c(`L/s` = "L/s", `gal/min (gpm)` = "gal/min", `ft³/s (cfs)` = "ft³/s"),
        selected = "L/s",
        width = "200px"
      )
    ),
    column(
      1,
      shinyjs::disabled(
        dateInput(
          "start_date",
          label = "Start Date",
          value = Sys.Date(),
          autoclose = TRUE
        )
      ),
      shinyjs::disabled(
        dateInput(
          "end_date",
          label = "End Date",
          value = Sys.Date(),
          autoclose = TRUE
        )
      ),
      br(),
      shinyjs::disabled(
        actionButton("submit", "Submit")
      )
    ),
    column(
      1,
      shinyjs::disabled(
        shinyTime::timeInput(
          "start_time",
          label = "Start Time",
          value = lubridate::hm("12:00"),
          seconds = FALSE
        )
      ),
      shinyjs::disabled(
        shinyTime::timeInput(
          "end_time",
          label = "End Time",
          value = lubridate::hm("12:00"),
          seconds = FALSE
        )
      )
    ),
    column(
      2,
      align = "left",
      br(),
      actionButton("reset_button", "Reload Application", width = "175px"),
      br(),
      br(),
      br(),
      shinyjs::disabled(
        actionButton("redraw_graph", "Redraw Graph(s)", width = "175px")
      )
    ),
    column(
      2,
      shinyWidgets::actionBttn(
        "feedback",
        "Feedback Form",
        width = '250px',
        onclick ="window.open('https://forms.office.com/pages/responsepage.aspx?id=PfKopOEaHEuZAuqhUwKBkNb1vpfauiZNit2g-l_MjnRUNVJWVFlFRzdLOVVPODlYMllLNjE3RU44Vy4u&web=1&wdLOR=cBEFC20B7-3BF7-4F6B-ADE9-65CD584DA1A3', '_blank')"
      ),
      shinyWidgets::actionBttn(
        "apidoc",
        "Access the code & documentation \n on GitHub",
        width = '250px',
        onclick ="window.open('https://github.com/SCCWRP/FWCCalculator', '_blank')"
      ),
    ),
    column(
      2,
      div()
    )
  ),
  fluidRow(
    column(
      4
    ),
    column(
      4,
      textInput(
        inputId = "title",
        label = "Graph Title",
        placeholder = "Enter an optional title for the graph(s)",
        value = "",
        width = "500px"
      )
    ),
    column(
      4
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
            Download the Excel template file ",), downloadLink("download_template", label = "here"), HTML(" and overwrite it with your data. See the Data Requirements section below. <br><strong>NOTE</strong>: a 'download.htm' file may be downloaded instead of the template Excel file if the link is clicked too soon after launching the application. This is a known issue with the 'shiny' R package which was used to develop this application. Please allow a few minutes before downloading the template.
          </li>
          <li>
            Upload your data by clicking the 'Browse' button, selecting the updated Excel spreadsheet, and clicking the 'Submit' button. The calculator will generate the aliquot volume table as well as the hydrograph and pollutograph(s), depending on the uploaded data. If pollutant data is provided, the calculator will also provide the Event Mean Concentration for each of the specified pollutants.
          </li>
          <li>
            Use the 'Start Date/Time' and 'End Date/Time' inputs to filter the data to the appropriate time range. The grayed-out sections of the graph will <i>not</i> be included in the aliquot volume and event mean concentration calculations.
          </li>
          <li>
            After changing the 'Start Date/Time' and 'End Date/Time' inputs, click the 'Redraw Graph(s)' button to regenerate the aliquot volume table, hydrograph, and pollutograph(s), filtered to the provided times.
          </li>
          <li>
            The 'Composite Vol.' input is used in the aliquot volume calculation such that the sum of the aliquot volumes will be equal to the composite volume value entered here, measured in mL.
          </li>
          <li>
            The 'Flow Units of Submitted Data' input is used to label and calculate the 'Total Hydrograph Volume' output.
          </li>
        </ol>
           "),
      hr(),
      h3("Data Requirements"),
      HTML("The uploaded Excel spreadsheet must conform to the following requirements:
        <ul>
          <li>
            Must contain exactly two sheets, in the following order:
          </li>
          <ul>
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
        If you have any questions, please contact us at
          <a href="mailto:stormwater@sccwrp.org?subject=Flow-Weighting and Compositing Calculator Question">stormwater@sccwrp.org</a>.
        '
      )
    ),
    id = "full_page"
  )
)
