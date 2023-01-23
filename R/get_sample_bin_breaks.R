get_sample_bin_breaks <- function(joined, flow, attr = "center") {
  mins <- unique(joined$mins) |>
    sort()

  sample_bin_breaks <- numeric(length(mins) + 1)
  sample_bin_breaks[1] <- flow$mins[1]
  for (i in 2:length(mins)) {
    sample_bin_breaks[i] <- mean(c(mins[i-1], mins[i]))
  }
  sample_bin_breaks[length(sample_bin_breaks)] <- max(flow$mins)

  sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)
  sample_bin_breaks
}
