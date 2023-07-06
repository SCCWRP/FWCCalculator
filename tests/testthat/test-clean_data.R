flow <- readxl::read_excel("test_data/FashionValley_two_conc.xlsx", sheet = 2)
sample <- readxl::read_excel("test_data/FashionValley_two_conc.xlsx", sheet = 3)


test_that("flow is correct", {
  expect_equal(clean_data(flow, sample)$flow, FashionValleyFlow)
})

test_that("sample is correct", {
  expect_equal(clean_data(flow, sample)$sample, FashionValleySample)
})

test_that("joined is correct", {
  expect_equal(clean_data(flow, sample)$joined, FashionValleyJoined)
})
