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
ts_train_msts <- msts(
  train_data$daily_avg,
  seasonal.periods = c(7, 365)
)

# validation series only for plotting
ts_valid_msts <- msts(
  valid_data$daily_avg,
  seasonal.periods = c(7, 365)
)

# quick plot of training series
autoplot(ts_train_msts) +
  ggtitle("Training Daily Electricity Load")

# baseline model: TBATS
fit_tbats <- tbats(ts_train_msts)

# forecast 31 days for validation period
fc_tbats <- forecast(fit_tbats, h = 31)

# plot forecast vs actual validation data
autoplot(fc_tbats) +
  autolayer(ts_valid_msts, series = "Actual") +
  ggtitle("TBATS Forecast vs Actuals")

# accuracy on validation set
accuracy(as.numeric(fc_tbats$mean), valid_data$daily_avg)

# current MAPE is 11.31%, retrain on all data and generate one sample submission
full_train_data <- data_all %>%
  filter(date >= as.Date("2005-01-01"),
         date <= as.Date("2011-06-30"))

ts_full_msts <- msts(
  full_train_data$daily_avg,
  seasonal.periods = c(7, 365.25)
)

final_fit <- tbats(
  ts_full_msts,
  use.box.cox = NULL,
  use.trend = TRUE,
  use.damped.trend = TRUE
)

final_fc <- forecast(final_fit, h = 31)

# build submission file

submission <- data.frame(
  date = seq(as.Date("2011-07-01"), as.Date("2011-07-31"), by = "day"),
  load = as.numeric(final_fc$mean)
)
dir.create("Output", showWarnings = FALSE)
write.csv(
  submission,
  file = "Output/submission_basic_tbats.csv",
  row.names = FALSE,
  quote = TRUE
)
