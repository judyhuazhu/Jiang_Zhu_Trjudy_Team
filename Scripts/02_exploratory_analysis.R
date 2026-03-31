library(tidyverse)
library(forecast)

# load processed data
source("Scripts/01_read_all_data.R")

# create msts object
ts_load <- msts(
  data_all$daily_avg,
  seasonal.periods = c(7, 365)
)

autoplot(ts_load) +
  ggtitle("Daily Electricity Load")