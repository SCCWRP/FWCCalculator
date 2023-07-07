joined <- FashionValleyJoined
flow <- FashionValleyFlow

excel_breaks <- c(0, 1860, 1920, 1980, 2040, 2100, 2160, 2220, 2280, 2340, 2400, 2460, 2520, 2580, 2640, 2700, 2760, 2820, 2880, 2940, 3000, 3060, 3120, 3180, 9660)

test_that("sample bin breaks are correct", {
  expect_equal(get_sample_bin_breaks(joined, flow), excel_breaks)
})


# excel_breaks <- c(0, 1860, 1920, 1980, 2040, 2100, 2160, 2220, 2280, 2340, 2400, 2460, 2520, 2580, 2640, 2700, 2760, 2820, 2880, 2940, 3000, 3060, 3120, 3180, 9660)
#
# test_that("sample bin breaks are correct", {
#   expect_equal(get_sample_bin_breaks(joined, flow), excel_breaks)
# })
