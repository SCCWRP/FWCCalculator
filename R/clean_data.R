clean_data <- function(flow, sample) {
  flow_out <- flow |>
    rename_with(.fn = function(x) "times", .cols = 1) |>
    mutate(times = as.POSIXct(trunc(times))) |>
    arrange(times) |>
    mutate(mins = lubridate::time_length(times - times[1], unit = 'mins')) |>
    reshape2::melt(id.vars = c('mins', 'times'), variable.name = "rate", value.name = "flow_values") |>
    distinct()

  sample_out <- sample |>
    rename_with(.fn = function(x) "times", .cols = 1) |>
    mutate(times = as.POSIXct(trunc(times))) |>
    arrange(times) |>
    mutate(mins = lubridate::time_length(times - flow_out$times[1], unit = 'mins')) |>
    reshape2::melt(id.vars = c('mins', 'times'), variable.name = "conc", value.name = "conc_values") |>
    distinct()

  joined <- sample_out |>
    left_join(flow_out, by = c('times' = 'times'), suffix = c('_sample', '_flow')) |>
    transmute(
      times,
      values = if_else(
        is.na(flow_values),
        purrr::map_dbl(mins_sample, estimate_flow, flow = flow_out),
        flow_values
      ),
      mins = mins_sample,
    ) |>
    distinct()

  list(flow = flow_out, sample = sample_out, joined = joined)
}


