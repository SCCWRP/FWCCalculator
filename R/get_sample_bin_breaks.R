get_sample_bin_breaks <- function(joined, flow) {
  if(nrow(joined) == nrow(flow)) {
    return(joined$mins)
  } else if(nrow(joined) == 1) {
    return(c(min(flow$mins), max(flow$mins)))
  }

  mins <- unique(joined$mins)


  sample_bin_breaks <- stats::filter(mins, rep(1/2, 2), sides = 2)

  sample_bin_breaks[length(sample_bin_breaks)] = max(flow$mins)

  sample_bin_breaks <- c(flow$mins[1], sample_bin_breaks)

  sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)
  sample_bin_breaks
}
