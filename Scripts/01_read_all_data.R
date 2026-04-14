library(readxl)
library(tidyverse)
library(here)
read_excel(here("Data", "load.xlsx"))
load <- read_excel("Data/load.xlsx")
temp <- read_excel("Data/temperature.xlsx")
humidity <- read_excel("Data/relative_humidity.xlsx")

# check structure
str(load)
str(temp)
str(humidity)

# check first few rows
head(load)
head(temp)
head(humidity)

# process load to daily average
daily_load <- load %>%
  mutate(
    date = as.Date(date),
    daily_avg = rowMeans(select(., starts_with("h")), na.rm = TRUE)
  ) %>%
  select(date, daily_avg)

head(daily_load)
str(daily_load)

# process temperature
daily_temp <- temp %>%
  mutate(
    date = as.Date(date),
    temp_avg_hour = rowMeans(select(., starts_with("t_ws")), na.rm = TRUE)
  ) %>%
  group_by(date) %>%
  summarise(daily_temp = mean(temp_avg_hour, na.rm = TRUE)) %>%
  ungroup()

head(daily_temp)
str(daily_temp)

# process humidity
daily_humidity <- humidity %>%
  mutate(
    date = as.Date(date),
    humidity_avg_hour = rowMeans(select(., starts_with("rh_ws")), na.rm = TRUE)
  ) %>%
  group_by(date) %>%
  summarise(daily_humidity = mean(humidity_avg_hour, na.rm = TRUE)) %>%
  ungroup()

head(daily_humidity)
str(daily_humidity)

# check NA in temp
sum(is.na(daily_temp$date))

# inspect problematic rows
daily_temp %>%
  filter(is.na(date))

daily_temp <- daily_temp %>%
  filter(!is.na(date))

sum(is.na(daily_temp$date))
range(daily_temp$date)
dim(daily_temp)

# merge all datasets
data_all <- daily_load %>%
  left_join(daily_temp, by = "date") %>%
  left_join(daily_humidity, by = "date")

head(data_all)
str(data_all)
colSums(is.na(data_all))

