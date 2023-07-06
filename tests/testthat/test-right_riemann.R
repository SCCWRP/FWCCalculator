flow <- FashionValleyFlow
joined <- FashionValleyJoined


sample_bin_breaks <- get_sample_bin_breaks(joined, flow)

excel_calcs <- c(3233430, 223290, 223830, 244440, 263250, 288810, 309330, 322380, 325800, 323730, 316170, 306000, 296100, 284220, 271980, 260100, 249120, 239580, 230130, 222660, 245340, 379260, 434700, 55971990)


test_that("right riemann sum works", {
  expect_equal(right_riemann(sample_bin_breaks, flow, time_unit = "s"), excel_calcs)
})
