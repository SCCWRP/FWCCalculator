# file validator functions will return an error string when failing validation,
# so can check the type of the function value == "character" to determine
# failing validation, check for NULL when passing validation

good_filetype <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
bad_filetype <- data.frame(datapath = "test_data/bad_file_type.txt")

test_that("filetype validation works", {
  expect_null(is_correct_filetype(good_filetype))
  expect_type(is_correct_filetype(bad_filetype), "character")
})

one_sheet_file <- data.frame(datapath = "test_data/FashionValley_two_conc_one_sheet.xlsx")
three_sheet_file <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
four_sheet_file <- data.frame(datapath = "test_data/FashionValley_two_conc_four_sheets.xlsx")

test_that("file has 3 sheets", {
  expect_null(has_three_sheets(three_sheet_file))
  expect_type(has_three_sheets(one_sheet_file), "character")
  expect_type(has_three_sheets(four_sheet_file), "character")
})

one_column_file <- data.frame(datapath = "test_data/FashionValley_two_conc_one_column.xlsx")
two_column_file <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
three_column_file <- data.frame(datapath = "test_data/FashionValley_two_conc_three_columns.xlsx")

test_that("flow sheet in file has 2 columns", {
  expect_null(has_two_columns(two_column_file))
  expect_type(has_two_columns(one_column_file), "character")
  expect_type(has_two_columns(three_column_file), "character")
})

no_missing_values <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
missing_values <- data.frame(datapath = "test_data/FashionValley_two_conc_missing_values.xlsx")

test_that("missing value check works", {
  expect_null(has_no_missing_values(no_missing_values, sheet = 2))
  expect_type(has_no_missing_values(missing_values, sheet = 2), "character")
  expect_null(has_no_missing_values(no_missing_values, sheet = 3))
  expect_type(has_no_missing_values(missing_values, sheet = 3), "character")
})

no_negative_values <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
negative_values_both_sheets <- data.frame(datapath = "test_data/FashionValley_two_conc_lt_zero.xlsx")
negative_values_one_sheet <- data.frame(datapath = "test_data/FashionValley_two_conc_lt_zero_one_sheet.xlsx")

test_that("negative value check works", {
  expect_null(has_no_negative_values(no_negative_values))
  expect_type(has_no_negative_values(negative_values_both_sheets), "character")
  expect_type(has_no_negative_values(negative_values_one_sheet), "character")
})

correct_date_format <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
incorrect_date_format <- data.frame(datapath = "test_data/FashionValley_two_conc_date_wrong_format.xlsx")

test_that("date format check works", {
  expect_null(has_correct_date_format(correct_date_format, sheet = 2))
  expect_type(has_correct_date_format(incorrect_date_format, sheet = 2), "character")
  expect_null(has_correct_date_format(correct_date_format, sheet = 3))
  expect_type(has_correct_date_format(incorrect_date_format, sheet = 3), "character")
})

correct_measurement_format <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
incorrect_measurement_format <- data.frame(datapath = "test_data/FashionValley_two_conc_bad_measure_data.xlsx")

test_that("measurement format check works", {
  expect_null(has_correct_measurement_format(correct_measurement_format, sheet = 2))
  expect_type(has_correct_measurement_format(incorrect_measurement_format, sheet = 2), "character")
  expect_null(has_correct_measurement_format(correct_measurement_format, sheet = 3))
  expect_type(has_correct_measurement_format(incorrect_measurement_format, sheet = 3), "character")
})

headers_present <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
no_headers <- data.frame(datapath = "test_data/FashionValley_two_conc_no_headers.xlsx")

test_that("headers check works", {
  expect_null(has_headers(headers_present, sheet = 2))
  expect_type(has_headers(no_headers, sheet = 2), "character")
  expect_null(has_headers(headers_present, sheet = 3))
  expect_type(has_headers(no_headers, sheet = 3), "character")
})

good_sample_range <- data.frame(datapath = "test_data/FashionValley_two_conc.xlsx")
bad_sample_range <- data.frame(datapath = "test_data/FashionValley_two_conc_samples_out_of_range.xlsx")

test_that("sample range validation", {
  expect_null(has_sample_times_in_range(good_sample_range))
  expect_type(has_sample_times_in_range(bad_sample_range), "character")
})

