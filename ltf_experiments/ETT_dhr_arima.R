library(fable)

# This is the DHR-ARIMA model for the ETTh1 data
# This script takes as input the number of the input data file

args <- commandArgs(trailingOnly=TRUE)
file_number <- args[1]

horizon <- 720

horizon <- as.numeric(horizon)

data <- readr::read_csv(paste0("./data/ETT/processed/ETTh1_window_", file_number, ".csv"))
data <- data %>% as_tsibble(index = date)

# perform DHR-ARIMA forecasting
model <- data %>% model(
  dhr_arima = ARIMA(OT ~ PDQ(0,0,0) + fourier(period = 24, K = 10) +
                  fourier(period = 24 * 365, K = 5)))
forecasts <- model %>% forecast(h=horizon)
forecasts <- forecasts$.mean
write.csv(forecasts, paste0("./results/ETT/dhr_arima/ETTh1_window_", file_number, "dhr_arima.csv"), row.names = FALSE)
