test_data <- list(flow = data.frame(times = c(as_datetime('2023-01-01 12:01:00 AM'),
                                              as_datetime('2023-01-01 12:02:00 AM'),
                                              as_datetime('2023-01-01 12:03:00 AM')),
                                    values = c(1,2,3)),
                  joined = data.frame(times = c(as_datetime('2023-01-01 12:01:00 AM'),
                                                as_datetime('2023-01-01 12:02:00 AM'),
                                                as_datetime('2023-01-01 12:03:00 AM')),
                                      values = c(1,2,3)),
                  units = "ft\u00B3/s")

test_that("conversion from ft³/s to L/s works", {
  expect_equal(volume_conversion(test_data, "L/s"), list(flow = c(28.3168, 56.6337, 84.9505), joined = c(28.3168, 56.6337, 84.9505)))
})
