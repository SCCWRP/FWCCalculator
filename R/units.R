allowed_units <- c("ft³/s", "L/s", "m³/s")

conversion_vector <- c(1, 28.3168, 1, 0.0283168, 0.001, 1) # upper triangle conversion factors by column


unit_conversion_matrix <- matrix(0, nrow = length(allowed_units), ncol <- length(allowed_units))
unit_conversion_matrix[upper.tri(unit_conversion_matrix, diag = TRUE)] <- sapply(conversion_vector, function(x) {1/x})
unit_conversion_matrix <- t(unit_conversion_matrix)
unit_conversion_matrix[upper.tri(unit_conversion_matrix, diag = TRUE)] <- conversion_vector

rownames(unit_conversion_matrix) <- allowed_units
colnames(unit_conversion_matrix) <- allowed_units
