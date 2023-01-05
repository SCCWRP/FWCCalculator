volume_conversion <- function(data, new_unit) {
  data$flow$values <- data$flow$values*unit_conversion_matrix[data$units, new_unit]
  data$joined$values <- data$joined$values*unit_conversion_matrix[data$units, new_unit]
  data$units <- new_unit
  data
}
