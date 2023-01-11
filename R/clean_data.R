clean_data <- function(flow, sample) {
  names(flow) <- c('times', 'values')
  names(sample)[1] <- 'times'


  flow <- flow |>
    mutate(times = as_datetime(times)) |>
    arrange(times) |>
    mutate(mins = time_length(times - times[1], unit = 'mins'))


  sample <- sample |>
    mutate(times = as_datetime(times)) |>
    arrange(times) |>
    mutate(mins = time_length(times - flow$times[1], unit = 'mins'))


  joined <- sample |>
    left_join(flow, by = c('times' = 'times'), suffix = c('_sample', '_flow')) |>
    transmute(
      times,
      values = if_else(
        is.na(values),
        map_dbl(mins_sample, estimate_flow, flow = flow),
        values
      ),
      mins = mins_sample,
    )

  list(flow = flow, sample = sample, joined = joined)
}


