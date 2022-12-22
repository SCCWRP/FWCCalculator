# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
source("R/calculate_bottle_proportions.R")
source("R/clean_data.R")
source("R/estimate_flow.R")
source("R/get_nearest_time.R")
source("R/volume_conversion.R")

test_dir("testthat")
