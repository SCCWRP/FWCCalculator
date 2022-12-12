source('R/estimate_flow.R', local = TRUE)

clean_data <- function(data_path) {
  flow <- read_excel(data_path, sheet = 1)
  sample <- read_excel(data_path, sheet = 2)
  names(flow) <- c('times', 'values')
  names(sample) <- c('times', 'values')

  time_format <- '%m-%d-%Y %H:%M:%S'
  flow <- flow |>
    mutate(times = as_datetime(times, format = time_format)) |>
    arrange(times) |>
    mutate(mins = time_length(times - times[1], unit = 'mins'))


  sample <- sample |>
    mutate(times = as_datetime(times, format = time_format)) |>
    arrange(times)


  joined <- sample |>
    left_join(flow, by = c('times' = 'times'), suffix = c('_sample', '_flow')) |>
    mutate(
      mins = if_else(
        is.na(mins),
        time_length(times - times[which.min(times)], unit = 'mins') + mins[which.min(mins)],
        mins
      ),
      values_flow = if_else(
        is.na(values_flow),
        map_dbl(mins, estimate_flow, flow = flow),
        values_flow
      )
    )

  list(flow = flow, sample = sample, joined = joined)
}


