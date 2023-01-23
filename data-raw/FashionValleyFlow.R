flow <- readxl::read_excel("data-raw/FashionValley_two_conc.xlsx", sheet = 1)
sample <- readxl::read_excel("data-raw/FashionValley_two_conc.xlsx", sheet = 2)

FashionValleyFlow <- flow |>
  dplyr::rename_with(.fn = function(x) "times", .cols = 1) |>
  dplyr::mutate(times = lubridate::as_datetime(times)) |>
  dplyr::arrange(times) |>
  dplyr::mutate(mins = lubridate::time_length(times - times[1], unit = 'mins')) |>
  reshape2::melt(id.vars = c('mins', 'times'), variable.name = "rate", value.name = "flow_values")



usethis::use_data(FashionValleyFlow, overwrite = TRUE)
