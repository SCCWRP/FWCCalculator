is_correct_filetype <- function(file) {
  if (tools::file_ext(file$datapath) == "xlsx") {
    return(NULL)
  }
  else {
    return("File must be a .xlsx file. Please use the template provided in the Instructions tab.")
  }
}

has_two_sheets <- function(file) {
  if (length(readxl::excel_sheets(file$datapath)) == 2) {
    return(NULL)
  }
  else {
    return("Incorrect number of sheets. Please use the template provided in the Instructions tab.")
  }
}

has_two_columns <- function(file) {
  flow <- readxl::read_excel(file$datapath, sheet = 1)

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
    return(paste0("Missing values present on sheet ", sheet, " on rows ", paste(sort(unique(which(is.na(data)) %% dim(data)[1] + 1)), collapse = ", "), "."))
  }
}

has_no_negative_values <- function(file, sheet) {
  data <- readxl::read_excel(file$datapath, sheet = sheet)

  if (!any(data < 0)) {
    return(NULL)
  }
  else {
    return(paste0("Negative values present on sheet ", sheet, " on rows ", paste(sort(unique(which(data < 0) %% dim(data)[1] + 1)), collapse = ", "), "."))
  }
}

has_correct_date_format <- function(file, sheet) {
  data <- readxl::read_excel(file$datapath, sheet = sheet)

  if (all(purrr::map(data, class)[[1]] == c("POSIXct", "POSIXt"))) {
    return(NULL)
  }
  else {
    return(paste0("Incorrect date format on sheet ", sheet, ". Please use the template provided in the Instructions tab."))
  }
}

