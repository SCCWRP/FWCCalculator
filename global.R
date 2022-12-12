library(dplyr)
library(lubridate)
library(purrr)
library(readxl)
library(ggplot2)
source('R/clean_data.R', local = TRUE)
source('R/calculate_bottle_proportions.R', local = TRUE)

data_path <- 'data/EMC_Template-preprocess_test.xlsx'
cleaned_data <- clean_data(data_path)
flow <- cleaned_data$flow
sample <- cleaned_data$sample
joined <- cleaned_data$joined

ymin <- min(flow$values) - sd(flow$values) # maybe just zero instead?
ymax <- max(flow$values) + sd(flow$values)
xmin <- min(flow$mins)
xmax <- max(flow$mins)
