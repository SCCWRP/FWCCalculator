flow <- FashionValleyFlow
joined <- FashionValleyJoined


sample_bin_breaks <- numeric(length(joined$mins) + 1)
sample_bin_breaks[1] <- 0

for (i in 2:(length(sample_bin_breaks))) {
  sample_bin_breaks[i] <- mean(c(joined$mins[i-1], joined$mins[i]))
}
sample_bin_breaks[length(sample_bin_breaks)] <- max(flow$mins)

sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)


excel_calcs <- c(3200490, 225540, 222660, 237960, 258300, 282960, 304650, 320310, 325080, 325080, 318240, 308700, 298710, 286830, 275220, 263160, 251550, 241920, 232470, 224370, 226980, 351810, 420300, 55984590)


test_that("left riemann sum works", {
  expect_equal(left_riemann(sample_bin_breaks, flow), excel_calcs)
})
