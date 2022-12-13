source('R/get_nearest_time.R', local = TRUE)

calculate_bottle_proportions <- function(flow, sample, joined, composite_vol = 1000) {
  flow_row_num <- dim(flow)[1]
  sample_row_num <- dim(sample)[1]
  joined_row_num <- dim(sample)[1]
  if (any(flow_row_num == 0, sample_row_num == 0, joined_row_num == 0)) {
    return(data.frame(BottleNumber = NA, `Proportions (mL)` = NA))
  }

  sample_bin_breaks <- numeric(length(joined$mins) + 1)
  sample_bin_breaks[1] <- 0

  for (i in 2:(length(sample_bin_breaks))) {
    sample_bin_breaks[i] <- mean(c(joined$mins[i-1], joined$mins[i]))
  }

  sample_bin_breaks[length(sample_bin_breaks)] <- max(flow$mins)

  sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)

  V <- numeric(length(sample$times))

  for (i in 1:length(V)) {
    x_bounds <- c(sample_bin_breaks[i], sample_bin_breaks[i + 1])

    flow_slices <- flow |>
      filter(mins %in% x_bounds[1]:x_bounds[2]) |>
      pull(values)

    min_slices <- flow |>
      filter(mins %in% x_bounds[1]:x_bounds[2]) |>
      pull(mins)

    slices <- length(flow_slices)

    vol <- 0
    for (j in 1:(slices-1)) {
      vol <- vol + mean(c(flow_slices[j], flow_slices[j + 1]))*(min_slices[j + 1] - min_slices[j])
    }
    V[i] <- vol
  }


  data.frame(BottleNumber = 1:length(V), `Proportions (mL)` = V/sum(V)*composite_vol)
}

