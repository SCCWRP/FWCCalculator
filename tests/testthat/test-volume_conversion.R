test_that("conversion from ft³/s to L/s works", {
  expect_equal(volume_conversion(c(1,2,3), "ft³/s", "L/s"), c(28.3168, 56.6337, 84.9505))
})
