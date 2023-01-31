flow <- FashionValleyFlow

test_that("flow interpolation calculations work", {
  expect_equal(estimate_flow(35, flow), 27.866667)
  expect_equal(estimate_flow(15, flow), 27.6)
  expect_equal(estimate_flow(0, flow), 27.6)
  expect_equal(estimate_flow(-10, flow), as.numeric(NA))
})
