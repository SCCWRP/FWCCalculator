global_font_size <- 18


# global units
allowed_flow_units <- c("ft\u00B3/s", "L/s", "m\u00B3/s")

conversion_vector <- c(1, 28.3168, 1, 0.0283168, 0.001, 1) # upper triangle conversion factors by column


unit_conversion_matrix <- matrix(0, nrow = length(allowed_flow_units), ncol <- length(allowed_flow_units))
unit_conversion_matrix[upper.tri(unit_conversion_matrix, diag = TRUE)] <- sapply(conversion_vector, function(x) {1/x})
unit_conversion_matrix <- t(unit_conversion_matrix)
unit_conversion_matrix[upper.tri(unit_conversion_matrix, diag = TRUE)] <- conversion_vector

rownames(unit_conversion_matrix) <- allowed_flow_units
colnames(unit_conversion_matrix) <- allowed_flow_units


allowed_concentration_units <- c("mg/L")
