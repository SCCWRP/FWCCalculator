flow <- FashionValleyFlow
joined <- FashionValleyJoined


sample_bin_breaks <- get_sample_bin_breaks(joined, flow)

excel_calcs <- c(3200490, 225540, 222660, 237960, 258300, 282960, 304650, 320310, 325080, 325080, 318240, 308700, 298710, 286830, 275220, 263160, 251550, 241920, 232470, 224370, 226980, 351810, 420300, 55984590)


test_that("left riemann sum works", {
  expect_equal(left_riemann(sample_bin_breaks, flow, time_unit = "s"), excel_calcs)
})
