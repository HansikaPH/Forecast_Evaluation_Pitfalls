library(fable)

# This is the DHR-ARIMA model for the ECL dataset
# This script takes as input the number of the input data file
args <- commandArgs(trailingOnly=TRUE)
file_number <- args[2]

horizon <- 960

data <- readr::read_csv(paste0("./data/ECL/processed/ECL_window_", file_number, ".csv"))
data <- data %>% as_tsibble(index = date)

# perform DHR-ARIMA forecasting
fit <- data %>% model(
  dhr_arima = ARIMA(MT_320 ~ PDQ(0,0,0) + fourier(period = 24, K = 10) +
                  fourier(period = 24 * 7, K = 5) + fourier(period = 24 * 365, K = 3)))
forecasts <- fit %>% forecast(h=horizon)
forecasts <- forecasts$.mean
write.csv(forecasts, paste0("./results/ECL/dhr_arima/ECL_window_", file_number, "dhr_arima.csv"), row.names = FALSE)