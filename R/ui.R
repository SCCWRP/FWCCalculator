ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$style(paste0('body {font-size: ', global_font_size, 'px}
                              .action-button {font-size: ', global_font_size, 'px}
                              #volume1 {font-weight: bold}
                              #volume2 {font-weight: bold}
                              #start_time {color: Gray}
                              #end_time {color: Gray}'))),

  titlePanel("Flow-Weighting and Compositing Calculator"),
  fluidRow(
    column(
      2,
      div()
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
        actionButton("submit", "Submit")
      )
    ),
    column(
      2,
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
      shinyjs::disabled(
        numericInput(
          "lower_mins",
          "Start Time (min)",
          min = 0,
          value = 0,
          step = 5,
          width = "200px"
        )
      ),
      textOutput("start_time")
    ),
    column(
      2,
      textInput(
        "vol_units",
        "Water Volume Units",
        "L",
        width = "200px"
      ),
      shinyjs::disabled(
        numericInput(
          "upper_mins",
          "End Time (min)",
          min = 0,
          step = 5,
          value = 10000,
          width = "200px"
        )
      ),
      textOutput("end_time")
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
        width = "455px"
      )
    ),
    column(
      4,
      div()
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
            Use the 'Start Time' and 'End Time' inputs to filter the data to the appropriate time range. Elapsed time in minutes is presented. The grayed-out sections of the graph will <i>not</i> be included in the aliquot volume and event mean concentration calculations.
          </li>
          <li>
            After changing the 'Start Time' and 'End Time', click the 'Redraw Graph(s)' button to regenerate the aliquot volume table, hydrograph, and pollutograph(s), filtered to the provided times.
          </li>
          <li>
            The 'Composite Vol.' input is used in the aliquot volume calculation such that the sum of the aliquot volumes will be equal to the composite volume value entered here, measured in mL.
          </li>
          <li>
            The 'Water Volume Units' input is used to accurately label the calculated 'Total Hydrograph Volume' output.
          </li>
        </ol>
           "),
      hr(),
      h3("Data Requirements"),
      HTML("The uploaded Excel spreadsheet must conform to the following requirements:
        <ul>
          <li>
            It must contain exactly two sheets, one for the flow rate measurement data, and one for the sample timestamps/pollutant measurement data.
          </li>
          <li>
            The first sheet must have exactly two columns, one for the timestamps and one for the flow rate measurements.
          </li>
          <li>
            The first column of each sheet must be timestamps with both date and time in the 'mm/dd/yy  hh:mm:ss' format. The 'Datetime' columns in the provided template file are already in the correct format.
          </li>
          <li>
            Any number of pollutant columns in the second sheet are supported. If you do not have pollutant data, delete the 'Pollutant' columns entirely before uploading the template.
          </li>
          <li>
            The column headers are required and can be renamed as needed, but cannot be exclusively numeric characters [0-9]. The flow rate and pollutant column headers will be used for axis titles and can contain the units of the measurements, for example.
          </li>
          <li>
            All flow rate and pollutant measurements must be greater than zero.
          </li>
          <li>
            There may not be any missing values in the spreadsheet.
          </li>
        </ul>")#,
      # hr(),
      # h3("Technical Details"),
      # HTML("The hydrograph volume is calculated by a trapezoidal approximation, with partitions set at every data point. The aliquot volume values are allocated to each sample timestamp by using the area under the curve between the midpoint of the previous sample and the current sample and the current sample and the next sample. For example if we have samples 1, 2, and 3 at times 10, 14, and 26, then the area under the curve between times 12 and 20 will be allocated to sample 2, since the midpoint between 10 and 14 is 12, and the midpoint between 14 and 26 is 20.")
    ),
    id = "full_page"
  )
)
