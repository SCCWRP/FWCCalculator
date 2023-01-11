flow <- readxl::read_excel("data-raw/FashionValley_two_conc.xlsx", sheet = 1)

FashionValleyFlow <- flow |>
  dplyr::rename_with(.fn = function(x) "times", .cols = 1) |>
  dplyr::mutate(times = lubridate::as_datetime(times)) |>
  dplyr::arrange(times) |>
  dplyr::mutate(mins = lubridate::time_length(times - times[1], unit = 'mins'))


usethis::use_data(FashionValleyFlow, overwrite = TRUE)
