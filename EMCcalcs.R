library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)

estimate_flow <- function(sample_min, flow) {
  # if the sample is taken between two recorded flow measurements, we need to
  # use a linear interpolation to match the sample time with an estimated flow
  prev_flow <- flow |>
    filter(mins < sample_min) |>
    select(mins, values) |>
    tail(1) |>
    as.numeric()

  next_flow <- flow |>
    filter(mins > sample_min) |>
    select(mins, values) |>
    head(1) |>
    as.numeric()

  estimated_flow <- (prev_flow[2]*(next_flow[1] - sample_min) + next_flow[2]*(sample_min - prev_flow[1]))/
    (next_flow[1] - prev_flow[1])
  estimated_flow
}

get_nearest_time <- function(sample_min, flow_mins) {
  flow_mins[which.min(abs(flow_mins-sample_min))]
}


flow <- read_excel('data/EMC_Template-preprocess.xlsx', sheet = 1)
sample <- read_excel('data/EMC_Template-preprocess.xlsx', sheet = 2)
names(flow) <- c('times', 'values')
names(sample) <- c('times', 'values')

time_format <- '%m-%d-%Y %H:%M:%S'
flow <- flow |>
  mutate(times = as_datetime(times, format = time_format)) |>
  arrange(times) |>
  mutate(mins = time_length(times - times[1], unit = 'mins'))


sample <- sample |>
  mutate(times = as_datetime(times, format = time_format)) |>
  arrange(times)


joined <- sample |>
  left_join(flow, by = c('times' = 'times'), suffix = c('_sample', '_flow')) |>
  mutate(
    mins = if_else(
      is.na(mins),
      time_length(times - times[which.min(times)], unit = 'mins') + mins[which.min(mins)],
      mins
    ),
    values_flow = if_else(
      is.na(values_flow),
      map_dbl(mins, estimate_flow, flow = flow),
      values_flow
    )
  )



concat_mins <- c(0, joined$mins, max(flow$mins))
sample_bin_breaks <- numeric(length(concat_mins))


for (i in 2:(length(concat_mins)-1)) {
  sample_bin_breaks[i] <- mean(c(concat_mins[i], concat_mins[i+1]))
}

sample_bin_breaks[length(concat_mins)] <- tail(concat_mins,1)

sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)


V <- numeric(length(sample$times))


for (i in 1:length(V)) {
  x_bounds <- c(sample_bin_breaks[i], sample_bin_breaks[i + 1])
  flow_slices <-flow |>
    filter(mins %in% x_bounds[1]:x_bounds[2]) |>
    pull(values)
  min_slices <- flow |>
    filter(mins %in% x_bounds[1]:x_bounds[2]) |>
    pull(mins)
  slices <- length(flow_slices)
  vol <- 0
  for (j in 1:(slices-1)) {
    vol <- vol + mean(c(flow_slices[j], flow_slices[j + 1]))*(min_slices[j + 1] - min_slices[j])

  }
  print(paste0(i, "th length: ", slices))
  V[i] <- vol
}


V/sum(V)*1000



ggplot() +
  geom_point(data = joined, aes(x = mins, y = values_flow, color = 'Sample')) +
  geom_line(data = flow, aes(x = mins, y = values, color = 'Flow')) +
  geom_vline(xintercept = sample_bin_breaks) +
  geom_vline(xintercept = 1636, color = 'red') +
  ylim(min(flow$values) - sd(flow$values),max(flow$values) + sd(flow$values)) +
  labs(x = 'Time since start (min)', y = 'Flowrate', color = NULL) +
  theme(legend.position = c(.925,.95), legend.background = element_blank(), legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(shape = c(NA,16), linetype = c(1,NA))))
