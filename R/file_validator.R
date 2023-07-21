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

  problem_sheet <- readxl::excel_sheets(file$datapath)[2]

  if (dim(flow)[2] == 2) {
    return(NULL)
  }
  else {
    return(paste0("Incorrect number of columns on flow measurements sheet ", problem_sheet, ". Sheet should have only 2 columns: datetime and flow measurements."))
  }
}

has_no_missing_values <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[2:3]

  problem_sheets <- lapply(
    sheets,
    function(sheet) {
      data <- readxl::read_excel(file$datapath, sheet = sheet)
      if(any(is.na(data))) {
        sort(unique(which(is.na(data)) %% dim(data)[1] + 1))
      }
    }
  )

  names(problem_sheets) <- sheets

  problem_sheets <- problem_sheets[!(sapply(problem_sheets, is.null))]

  if (all(sapply(problem_sheets, is.null))) {
    return(NULL)
  }
  else {
    error_msg <- paste0("Missing value(s) present on sheet ", names(problem_sheets), " on row(s) ", sapply(problem_sheets, paste, collapse = ", "), ".", collapse = "\n")
    error_msg <- paste(error_msg, "Please correct data and submit again.")
    return(error_msg)
  }
}

has_no_negative_values <- function(file) {
  #browser()
  sheets <- readxl::excel_sheets(file$datapath)[2:3]

  problem_sheets <- lapply(
    sheets,
    function(sheet) {
      data <- readxl::read_excel(file$datapath, sheet = sheet)
      if(any(data[, -1] < 0)) {
        sort(unique(which(data[, -1] < 0) %% dim(data)[1] + 1))
      }
    }
  )

  names(problem_sheets) <- sheets

  problem_sheets <- problem_sheets[!(sapply(problem_sheets, is.null))]

  if (all(sapply(problem_sheets, is.null))) {
    return(NULL)
  }
  else {
    error_msg <- paste0("Negative value(s) present on sheet ", names(problem_sheets), " on row(s) ", sapply(problem_sheets, paste, collapse = ", "), ".", collapse = "\n")
    error_msg <- paste(error_msg, "Please correct data and submit again.")
    return(error_msg)
  }
}

has_correct_date_format <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[2:3]

  problem_sheets <- lapply(
    sheets,
    function(sheet) {
      data <- readxl::read_excel(file$datapath, sheet = sheet)
      !(all(purrr::map(data, class)[[1]] == c("POSIXct", "POSIXt")))
    }
  )

  names(problem_sheets) <- sheets


  if (all(sapply(problem_sheets, isFALSE))) {
    return(NULL)
  }
  else {
    error_msg <- paste0("Incorrect date format on sheet ", names(problem_sheets), ".", collapse = "\n")
    error_msg <- paste(error_msg, "Please correct data and submit again.")
    return(error_msg)
  }
}

has_correct_measurement_format <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[2:3]

  problem_sheets <- lapply(
    sheets,
    function(sheet) {
      # purrr::map returns a list of the classes of data, which is desirable since the first column of dates
      # will have more than 2 classes
      # then use do.call to iterate through the list and concatenate the resulting classes to ensure they're
      # all numeric typed, except the first column
      data <- readxl::read_excel(file$datapath, sheet = sheet)
      !all(do.call(c, purrr::map(data, class)[-1]) == "numeric")
    }
  )

  names(problem_sheets) <- sheets


  if (all(sapply(problem_sheets, isFALSE))) {
    return(NULL)
  }
  else {
    error_msg <- paste0("Non-numeric data on sheet ", names(problem_sheets), ".", collapse = "\n")
    error_msg <- paste(error_msg, "Please correct data and submit again.")
    return(error_msg)
  }
}

has_headers <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[2:3]

  problem_sheets <- lapply(
    sheets,
    function(sheet) {
      # purrr::map returns a list of the classes of data, which is desirable since the first column of dates
      # will have more than 2 classes
      # then use do.call to iterate through the list and concatenate the resulting classes to ensure they're
      # all numeric typed, except the first column
      data <- readxl::read_excel(file$datapath, sheet = sheet)
      headers <- names(data)
      suppressWarnings(!all(is.na(as.numeric(headers))))
    }
  )

  names(problem_sheets) <- sheets


  if (all(sapply(problem_sheets, isFALSE))) {
    return(NULL)
  }
  else {
    error_msg <- paste0("Missing or incorrectly-formatted header(s) on sheet ", names(problem_sheets), ".", collapse = "\n")
    error_msg <- paste(error_msg, "The column headers are required and can be renamed as needed, but cannot be exclusively numeric characters [0-9]. Please correct data and submit again.")
    return(error_msg)
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

  all_samples_after_start <- all(lubridate::time_length(pull(sample[, 1]) - start_time_flow, unit = 'min') >= 0)
  all_samples_before_end <- all(lubridate::time_length(end_time_flow - pull(sample[, 1]), unit = 'min') >= 0)

  if (all_samples_after_start & all_samples_before_end) {
    return(NULL)
  }
  else {
    return("Not all sample timestamps within flow measurement timestamp range. Please adjust sample timestamps or flow measurement timestamps accordingly.")
  }
}
