trapezoid <- function(sample_bin_breaks, flow, time_unit) {
  bins <- 1:(length(sample_bin_breaks)-1)

  time_multiplier <- 1

  if(time_unit == "s") {
    time_multiplier <- 60
  }

  df_list <- lapply(bins, function(x) {
      flow[which(flow$mins %in% sample_bin_breaks[x]:sample_bin_breaks[x+1]), c("mins", "flow_values")]
    }
  )

  sapply(df_list, function(x) {
      ma <- stats::filter(x[, "flow_values"], rep(1/2, 2), sides = 2)
      diff(x[, "mins"]*time_multiplier) %*% ma[1:(length(ma)-1)]
    }
  )
}
