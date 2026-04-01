library(tidyverse)
library(forecast)

# load processed data
source("Scripts/01_read_all_data.R")

# split data based on instructor prompt
train_data <- data_all %>%
  filter(date >= as.Date("2005-01-01") &
           date <= as.Date("2010-06-30"))

valid_data <- data_all %>%
  filter(date >= as.Date("2010-07-01") &
           date <= as.Date("2010-07-31"))

# create msts object for training data
ts_train <- msts(
  train_data$daily_avg,
  seasonal.periods = c(7, 365)
)

# validation series only for plotting
ts_valid <- msts(
  valid_data$daily_avg,
  seasonal.periods = c(7, 365)
)

# quick plot of training series
autoplot(ts_train) +
  ggtitle("Training Daily Electricity Load")

# baseline model: TBATS
fit_tbats <- tbats(ts_train)

# forecast 31 days for validation period
fc_tbats <- forecast(fit_tbats, h = 31)

# plot forecast vs actual validation data
autoplot(fc_tbats) +
  autolayer(ts_valid, series = "Actual") +
  ggtitle("TBATS Forecast vs Actuals")

# accuracy on validation set
accuracy(as.numeric(fc_tbats$mean), valid_data$daily_avg)

