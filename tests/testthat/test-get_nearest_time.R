flow_mins <- FashionValleyFlow$mins

test_that("closest time is correct", {
  expect_equal(get_nearest_time(10, flow_mins), 15)
  expect_equal(get_nearest_time(5, flow_mins), 0)
  expect_equal(get_nearest_time(-5, flow_mins), 0)
  expect_equal(get_nearest_time(10000, flow_mins), 9660)
})
