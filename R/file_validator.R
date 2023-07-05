is_correct_filetype <- function(file) {
  if (tools::file_ext(file$datapath) == "xlsx") {
    return(NULL)
  }
  else {
    return("File must be a .xlsx file. Please use the template provided in the Instructions tab.")
  }
}

has_three_sheets <- function(file) {
  if (length(readxl::excel_sheets(file$datapath)) == 3) {
    return(NULL)
  }
  else {
    return("Incorrect number of sheets. Please use the template provided in the Instructions tab.")
  }
}

has_two_columns <- function(file) {
  flow <- readxl::read_excel(file$datapath, sheet = 2)

  if (dim(flow)[2] == 2) {
    return(NULL)
  }
  else {
    return("Incorrect number of columns in the flow measurements tab. Tab should have only 2 columns: datetime and flow measurements.")
  }
}

has_no_missing_values <- function(file, sheet) {
  data <- readxl::read_excel(file$datapath, sheet = sheet)

  if (!any(is.na(data))) {
    return(NULL)
  }
  else {
    problem_sheet <- readxl::excel_sheets(file$datapath)[sheet]
    return(paste0("Missing values present on sheet ", problem_sheet, " on row(s) ", paste(sort(unique(which(is.na(data)) %% dim(data)[1] + 1)), collapse = ", "), "."))
  }
}

has_no_negative_values <- function(file, sheet) {
  data <- readxl::read_excel(file$datapath, sheet = sheet)

  if (!any(data < 0)) {
    return(NULL)
  }
  else {
    problem_sheet <- readxl::excel_sheets(file$datapath)[sheet]
    return(paste0("Negative values present on sheet ", problem_sheet, " on row(s) ", paste(sort(unique(which(data < 0) %% dim(data)[1] + 1)), collapse = ", "), "."))
  }
}

has_correct_date_format <- function(file, sheet) {
  data <- readxl::read_excel(file$datapath, sheet = sheet)

  if (all(purrr::map(data, class)[[1]] == c("POSIXct", "POSIXt"))) {
    return(NULL)
  }
  else {
    problem_sheet <- readxl::excel_sheets(file$datapath)[sheet]
    return(paste0("Incorrect date format on sheet ", problem_sheet, ". Please use the template provided in the Instructions tab."))
  }
}

has_correct_measurement_format <- function(file, sheet) {
  data <- readxl::read_excel(file$datapath, sheet = sheet)

  # purrr::map returns a list of the classes of data, which is desirable since the first column of dates
  # will have more than 2 classes
  # then use do.call to iterate through the list and concatenate the resulting classes to ensure they're
  # all numeric typed, except the first column
  if (all(do.call(c, purrr::map(data, class)[-1]) == "numeric")) {
    return(NULL)
  }
  else {
    problem_sheet <- readxl::excel_sheets(file$datapath)[sheet]
    return(paste0("Incorrect data type on sheet ", problem_sheet, ". Make sure all measurements have a numeric format."))
  }

}

has_headers <- function(file, sheet) {
  data <- readxl::read_excel(file$datapath, sheet = sheet)
  headers <- names(data)

  # if any column headers can be coerced to numeric then assume input data is missing headers
  if (suppressWarnings(all(is.na(as.numeric(headers))))) {
    return(NULL)
  }
  else {
    problem_sheet <- readxl::excel_sheets(file$datapath)[sheet]
    return(paste0("Missing or incorrectly-formatted header(s) on sheet ", problem_sheet, ". The column headers are required and can be renamed as needed, but cannot be exclusively numeric characters [0-9]."))
  }
}

has_sample_times_in_range <- function(file) {
  flow <- readxl::read_excel(file$datapath, sheet = 2)
  sample <- readxl::read_excel(file$datapath, sheet = 3)

  start_time_flow <- flow[, 1] |>
    utils::head(1) |>
    pull()
  end_time_flow <- flow[, 1] |>
    utils::tail(1) |>
    pull()

  all_samples_after_start <- all(lubridate::time_length(pull(sample[, 1]) - start_time_flow, unit = 'min') > 0)
  all_samples_before_end <- all(lubridate::time_length(end_time_flow - pull(sample[, 1]), unit = 'min') > 0)

  if (all_samples_after_start & all_samples_before_end) {
    return(NULL)
  }
  else {
    return("Not all sample timestamps within flow measurement timestamp range. Please adjust sample timestamps or flow measurement timestamps accordingly.")
  }
}
