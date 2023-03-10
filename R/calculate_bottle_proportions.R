calculate_bottle_proportions <- function(flow, joined, composite_vol = 1000, method = 'trapezoid') {
  flow_row_num <- dim(flow)[1]
  joined_row_num <- dim(joined)[1]

  output <- data.frame(SampleTime = NA, AliquotVolume = NA, Proportions = NA, Volume = NA)

  if (any(flow_row_num == 0, joined_row_num == 0)) {
    return(output)
  }

  sample_bin_breaks <- get_sample_bin_breaks(joined, flow)


  if (method == 'left_riemann') {
    V <- left_riemann(sample_bin_breaks, flow)
  }
  else if (method == 'right_riemann') {
    V <- right_riemann(sample_bin_breaks, flow)
  }
  else {
    V <- trapezoid(sample_bin_breaks, flow)
  }


  output <- data.frame(SampleTime = joined$times, AliquotVolume = V/sum(V)*composite_vol, Proportions = V/sum(V), Volume = V)
  output
}

