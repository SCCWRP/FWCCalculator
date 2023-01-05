quadratic <- function(sample_bin_breaks, flow) {
  V <- numeric(length(sample_bin_breaks) - 1)


  for (i in 1:length(V)) {
    x_bounds <- c(sample_bin_breaks[i], sample_bin_breaks[i + 1])

    flow_slices <- flow |>
      filter(mins %in% x_bounds[1]:x_bounds[2]) |>
      pull(values)

    second_slices <- flow |>
      filter(mins %in% x_bounds[1]:x_bounds[2]) |>
      mutate(seconds = mins*60) |>
      pull(seconds)

    slices <- length(flow_slices)

    vol <- 0


    if (slices %% 2 == 1) {
      for (j in seq(from = 1, to = slices-2, by = 2)) {
        start <- second_slices[j]
        end <- second_slices[j+2]

        quad_coefs <- coef(lm(flow_slices[j:(j+2)] ~ poly(second_slices[j:(j+2)], 2, raw = TRUE)))
        vol <- vol + (end-start)*(quad_coefs[1] +
                                  quad_coefs[2]/2*(start + end) +
                                  quad_coefs[3]/3*(start^2 + start*end + end^2))
      }
      V[i] <- vol
    }
    else {
      for (j in seq(from = 1, to = slices - 3, by = 2)) {
        start <- second_slices[j]
        end <- second_slices[j+2]

        quad_coefs <- coef(lm(flow_slices[j:(j+2)] ~ poly(second_slices[j:(j+2)], 2, raw = TRUE)))
        vol <- vol + (end-start)*(quad_coefs[1] +
                                    quad_coefs[2]/2*(start + end) +
                                    quad_coefs[3]/3*(start^2 + start*end + end^2))
      }
      start <- second_slices[j+2]
      end <- second_slices[j+3]

      quad_coefs <- coef(lm(flow_slices[(j+1):(j+3)] ~ poly(second_slices[(j+1):(j+3)], 2, raw = TRUE)))
      vol <- vol + (end-start)*(-quad_coefs[1] +
                                  quad_coefs[2]/2*(start + end) +
                                  quad_coefs[3]/3*(start^2 + start*end + end^2))
      V[i] <- vol
    }
  }
  V

}
