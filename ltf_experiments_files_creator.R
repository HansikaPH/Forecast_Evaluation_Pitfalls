library(fabletools)
library(dplyr)
library(slider)
library(tsibble)
library(ggplot2)
library(feasts)
library(fable)

####################### ETTh1 dataset
horizons <- c(24, 48, 168, 336, 720)
rolling_origin_sizes <- c(2848, 2816, 2688, 2528, 2144)

# read the dataset file
dataset <- readr::read_csv("./Informer/dataset/ETT/ETTh1.csv")
dataset <- dataset %>% select(date, OT)
data_ts <- dataset %>% as_tsibble(index = date)

# plot the time series to check for potential seasonalities
data_ts %>% autoplot(OT)
data_ts_last <- data_ts[(nrow(data_ts) - 24 * 14):nrow(data_ts), ]
data_ts_last %>% autoplot(OT)
data_ts %>% ACF(lag_max = 24 * 21) %>% autoplot()

for (i in c(5:length(horizons))){
  horizon <- horizons[i]
  ro_size <- rolling_origin_sizes[i]

  # rolling origin
  rolling_data <- data_ts %>% stretch_tsibble(.init=11520, .step=1)
  rolling_data <- rolling_data %>% filter(.id <= ro_size)

  # save the dataset to seperate files
  for (id in c(1:ro_size)){
    rolling_window <- rolling_data %>% filter(.id == id)
    rolling_window <- rolling_window %>% select(date, OT)
    write.csv(rolling_window, paste0("./dataset/ETT/H_", horizon, "/ETTh1_window_", id, ".csv"), row.names = FALSE)
  }
}

####################### ECL dataset
horizon <- 960
ro_size <- 4288

# read the dataset file
data <- readr::read_csv("./Informer/data/ECL.csv")
data <- data %>% select(date, MT_320)
data_ts <- data %>% as_tsibble(index = date)

# plot the time series to check for potential seasonalities
data_ts %>% autoplot(MT_320)
data_ts_last <- data_ts[(nrow(data_ts) - 24 * 14):nrow(data_ts), ]
data_ts_last %>% autoplot(MT_320)
data_ts %>% ACF(lag_max = 24 * 21) %>% autoplot()

# rolling origin
rolling_data <- data_ts %>% stretch_tsibble(.init=21044, .step=1)
rolling_data <- rolling_data %>% filter(.id <= ro_size)

# save the dataset to seperate files
for (id in c(1:ro_size)){
  rolling_window <- rolling_data %>% filter(.id == id)
  rolling_window <- rolling_window %>% select(date, MT_320)
  write.csv(rolling_window, paste0("./data/ECL/H_", horizon, "/ECL_window_", id, ".csv"), row.names = FALSE)
}