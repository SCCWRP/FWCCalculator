calculate_bottle_proportions <- function(flow, sample, joined, composite_vol = 1000, method = 'trapezoid') {
  flow_row_num <- dim(flow)[1]
  sample_row_num <- dim(sample)[1]
  joined_row_num <- dim(joined)[1]

  output <- data.frame(SampleTime = NA, AliquotVolume = NA, Proportions = NA, Volume = NA)

  if (any(flow_row_num == 0, sample_row_num == 0, joined_row_num == 0)) {
    return(output)
  }


  sample_bin_breaks <- numeric(length(joined$mins) + 1)
  sample_bin_breaks[1] <- 0

  for (i in 2:(length(sample_bin_breaks))) {
    sample_bin_breaks[i] <- mean(c(joined$mins[i-1], joined$mins[i]))
  }
  sample_bin_breaks[length(sample_bin_breaks)] <- max(flow$mins)

  sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)



  if (method == 'left_riemann') {
    V <- left_riemann(sample_bin_breaks, flow)
  }
  else if (method == 'right_riemann') {
    V <- right_riemann(sample_bin_breaks, flow)
  }
  else if (method == 'trapezoid') {
    V <- trapezoid(sample_bin_breaks, flow)
  }
  else {
    V <- quadratic(sample_bin_breaks, flow)
  }


  output <- data.frame(SampleTime = sample$times, AliquotVolume = V/sum(V)*composite_vol, Proportions = V/sum(V), Volume = V)
  output
}

