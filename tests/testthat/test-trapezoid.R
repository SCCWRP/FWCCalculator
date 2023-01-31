flow <- FashionValleyFlow
joined <- FashionValleyJoined


sample_bin_breaks <- get_sample_bin_breaks(joined, flow)
excel_volume_calcs <- c(3216960,224415,223245,241200,260775,285885,306990,321345,325440,324405,317205,307350,297405,285525,273600,261630,250335,240750,231300,223515,236160,365535,427500,55978290)


test_that("trapezoid approximation is mean of left and right Riemann sums", {
  expect_equal(
    trapezoid(sample_bin_breaks, flow),
    cbind(left_riemann(sample_bin_breaks, flow), right_riemann(sample_bin_breaks, flow)) |>
      rowMeans()
  )
})

test_that("trapezoid approximation matches excel volume calculations", {
  expect_equal(trapezoid(sample_bin_breaks, flow), excel_volume_calcs)
})
