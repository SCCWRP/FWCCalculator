right_riemann <- function(sample_bin_breaks, flow, time_unit) {
  V <- numeric(length(sample_bin_breaks)-1)

  for (i in 1:length(V)) {
    x_bounds <- c(sample_bin_breaks[i], sample_bin_breaks[i + 1])

    flow_slices <- flow |>
      filter(mins %in% x_bounds[1]:x_bounds[2]) |>
      pull(flow_values)

    if (time_unit == "s") {
      time_slices <- flow |>
        filter(mins %in% x_bounds[1]:x_bounds[2]) |>
        mutate(seconds = mins*60) |>
        pull(seconds)
    } else {
      time_slices <- flow |>
        filter(mins %in% x_bounds[1]:x_bounds[2]) |>
        pull(mins)
    }

    slices <- length(flow_slices)

    vol <- 0



    for (j in 1:(slices-1)) {
      vol <- vol + flow_slices[j + 1]*(time_slices[j + 1] - time_slices[j])
    }
    V[i] <- vol
  }
  V
}
