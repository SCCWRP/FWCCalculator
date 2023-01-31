estimate_flow <- function(sample_min, flow) {
  if (sample_min %in% flow$mins) {
    return(flow$flow_values[flow$mins == sample_min])
  }

  # if the sample is taken between two recorded flow measurements, we need to
  # use a linear interpolation to match the sample time with an estimated flow
  prev_flow <- flow |>
    filter(mins < sample_min) |>
    select(mins, flow_values) |>
    utils::tail(1) |>
    as.numeric()

  next_flow <- flow |>
    filter(mins > sample_min) |>
    select(mins, flow_values) |>
    utils::head(1) |>
    as.numeric()

  estimated_flow <- (prev_flow[2]*(next_flow[1] - sample_min) + next_flow[2]*(sample_min - prev_flow[1]))/
    (next_flow[1] - prev_flow[1])
  estimated_flow
}
