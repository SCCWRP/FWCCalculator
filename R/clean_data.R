clean_data <- function(flow, sample) {

  flow <- flow |>
    rename_with(.fn = function(x) "times", .cols = 1) |>
    mutate(times = lubridate::as_datetime(times)) |>
    arrange(times) |>
    mutate(mins = lubridate::time_length(times - times[1], unit = 'mins')) |>
    reshape2::melt(id.vars = c('mins', 'times'), variable.name = "rate", value.name = "flow_values")


  sample <- sample |>
    rename_with(.fn = function(x) "times", .cols = 1) |>
    mutate(times = lubridate::as_datetime(times)) |>
    arrange(times) |>
    mutate(mins = lubridate::time_length(times - flow$times[1], unit = 'mins')) |>
    reshape2::melt(id.vars = c('mins', 'times'), variable.name = "conc", value.name = "conc_values")


  joined <- sample |>
    left_join(flow, by = c('times' = 'times'), suffix = c('_sample', '_flow')) |>
    transmute(
      times,
      values = if_else(
        is.na(flow_values),
        purrr::map_dbl(mins_sample, estimate_flow, flow = flow),
        flow_values
      ),
      mins = mins_sample,
    ) |>
    distinct()

  list(flow = flow, sample = sample, joined = joined)
}


