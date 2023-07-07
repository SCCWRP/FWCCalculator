trapezoid <- function(sample_bin_breaks, flow, time_unit) {
  bins <- 1:(length(sample_bin_breaks)-1)

  df_list <- lapply(bins, function(x) {
      flow[which(flow$mins %in% sample_bin_breaks[x]:sample_bin_breaks[x+1]), c("mins", "flow_values")]
    }
  )

  if(time_unit == "s") {
    return(
      sapply(df_list, function(x) {
          ma <- stats::filter(x[, "flow_values"], rep(1/2, 2), sides = 2)
          diff(x[, "mins"]*60) %*% ma[1:(length(ma)-1)]
        }
      )
    )
  }

  sapply(df_list, function(x) {
      ma <- stats::filter(x[, "flow_values"], rep(1/2, 2), sides = 2)
      diff(x[, "mins"]) %*% ma[1:(length(ma)-1)]
    }
  )
}
