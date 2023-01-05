right_riemann <- function(sample_bin_breaks, flow) {
  V <- numeric(length(sample_bin_breaks)-1)

  for (i in 1:length(V)) {
    x_bounds <- c(sample_bin_breaks[i], sample_bin_breaks[i + 1])

    flow_slices <- flow |>
      filter(mins %in% x_bounds[1]:x_bounds[2]) |>
      pull(values)

    second_slices <- flow |>
      filter(mins %in% x_bounds[1]:x_bounds[2]) |>
      mutate(seconds = mins*60) |>
      pull(seconds)

    slices <- length(flow_slices)

    vol <- 0



    for (j in 1:(slices-1)) {
      vol <- vol + flow_slices[j + 1]*(second_slices[j + 1] - second_slices[j])
    }
    V[i] <- vol
  }
  V
}
