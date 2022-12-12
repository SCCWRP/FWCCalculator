get_nearest_time <- function(sample_min, flow_mins) {
  flow_mins[which.min(abs(flow_mins-sample_min))]
}
