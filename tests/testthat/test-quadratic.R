flow <- FashionValleyFlow
joined <- FashionValleyJoined


sample_bin_breaks <- numeric(length(joined$mins) + 1)
sample_bin_breaks[1] <- 0

for (i in 2:(length(sample_bin_breaks))) {
  sample_bin_breaks[i] <- mean(c(joined$mins[i-1], joined$mins[i]))
}
sample_bin_breaks[length(sample_bin_breaks)] <- max(flow$mins)

sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)





test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
