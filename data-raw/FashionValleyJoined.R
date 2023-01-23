flow <- readxl::read_excel("data-raw/FashionValley_two_conc.xlsx", sheet = 1)
sample <- readxl::read_excel("data-raw/FashionValley_two_conc.xlsx", sheet = 2)

flow <- flow |>
  dplyr::rename_with(.fn = function(x) "times", .cols = 1) |>
  dplyr::mutate(times = lubridate::as_datetime(times)) |>
  dplyr::arrange(times) |>
  dplyr::mutate(mins = lubridate::time_length(times - times[1], unit = 'mins')) |>
  reshape2::melt(id.vars = c('mins', 'times'), variable.name = "rate", value.name = "flow_values")


sample <- sample |>
  dplyr::rename_with(.fn = function(x) "times", .cols = 1) |>
  dplyr::mutate(times = lubridate::as_datetime(times)) |>
  dplyr::arrange(times) |>
  dplyr::mutate(mins = lubridate::time_length(times - flow$times[1], unit = 'mins')) |>
  reshape2::melt(id.vars = c('mins', 'times'), variable.name = "conc", value.name = "conc_values")


FashionValleyJoined <- sample |>
  dplyr::left_join(flow, by = c('times' = 'times'), suffix = c('_sample', '_flow')) |>
  dplyr::transmute(
    times,
    values = dplyr::if_else(
      is.na(flow_values),
      purrr::map_dbl(mins_sample, estimate_flow, flow = flow),
      flow_values
    ),
    mins = mins_sample,
  ) |>
  dplyr::distinct()

usethis::use_data(FashionValleyJoined, overwrite = TRUE)
