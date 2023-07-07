right_riemann <- function(sample_bin_breaks, flow, time_unit) {
  bins <- 1:(length(sample_bin_breaks)-1)

  df_list <- lapply(bins, function(x) {
      flow[which(flow$mins %in% sample_bin_breaks[x]:sample_bin_breaks[x+1]), c("mins", "flow_values")]
    }
  )

  if(time_unit == "s") {
    return(
      sapply(df_list, function(x) {
          diff(x[, "mins"]*60) %*% x[2:nrow(x), "flow_values"]
        }
      )
    )
  }

  sapply(df_list, function(x) {
      diff(x[, "mins"]) %*% x[2:nrow(x), "flow_values"]
    }
  )
}
