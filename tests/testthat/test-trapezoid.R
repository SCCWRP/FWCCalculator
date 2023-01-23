flow <- FashionValleyFlow
joined <- FashionValleyJoined


sample_bin_breaks <- get_sample_bin_breaks(joined, flow)

test_that("multiplication works", {
  expect_equal(trapezoid(sample_bin_breaks, flow),
               cbind(left_riemann(sample_bin_breaks, flow), right_riemann(sample_bin_breaks, flow)) |>
                 rowMeans()
               )
})
