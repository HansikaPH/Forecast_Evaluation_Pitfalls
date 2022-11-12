library(EMD)
library(randomForest)
library(tsibble)
library(fable)
library(dplyr)
library(feasts)

# This script performs the experiments related to data leakage on a simulated exchange rate series
myTS <- read.csv("./data/simulated_exchange_rate/ts.csv")

test_set_size <- 1000
forecast_horizon <- 20
no_horizons <- test_set_size/forecast_horizon
input_size <- forecast_horizon * 1.25

###### leakage experiment
emd_output <- emd(as.vector(myTS)$x)

# split into train and test sets
train_sets <- list()
for (i in 1:emd_output$nimf){
  data <- emd_output$imf[, i]
  data_train <- data[1:(length(data) - test_set_size)]
  data_test <- data[(length(data) - test_set_size):test_set_size]

  train_sets[[i]] <- data_train
}

# perform random forest modelling for the imfs
models <- list()
final_windows <- list()
for (i in 1:emd_output$nimf){
  embedded_data <- data.frame(embed(train_sets[[i]], (input_size + 1))[,c((input_size + 1):1),drop=FALSE])
  final_windows[[i]] <- embedded_data[nrow(embedded_data), 2:ncol(embedded_data)]
  modRF <- randomForest(x=embedded_data[,-ncol(embedded_data)], y=embedded_data[,ncol(embedded_data)])
  models[[i]] <- modRF
}

# perform recursive forecasting for the imfs
all_imf_predictions <- list()
for (imf in 1:emd_output$nimf){
  model <- models[[imf]]
  predictions <- c()
  for (horizon in c(1:no_horizons)){
    if (horizon == 1){
      final_window <- final_windows[[imf]]
    }else{
      forecast_index <- length(emd_output$imf[, imf]) - test_set_size + ((horizon - 1) * forecast_horizon)
      final_window <- emd_output$imf[(forecast_index - input_size + 1):forecast_index, imf]
      final_window <- t(as.matrix(final_window))
    }

    colnames(final_window) <- paste0("X", c(1:input_size))
    for (h in c(1:forecast_horizon)){
      pred <- predict(model, final_window)
      predictions <- c(predictions, pred)

      # update the final window
      final_window <- c(final_window[2:length(final_window)], pred)
      final_window <- t(as.matrix(final_window))
      colnames(final_window) <- paste0("X", c(1:input_size))
    }
  }
  all_imf_predictions[[imf]] <- predictions
}

# AR modelling for the residual
residue <- as_tibble(emd_output$residue)
residue <- residue %>% mutate(index=seq(as.Date("2015/01/01"), by = "day", length.out = nrow(residue)))
residue <- residue %>% as_tsibble(index = index)

rolling_windows <- stretch_tsibble(residue, .step = forecast_horizon, .init = (length(as.vector(myTS)$x) - test_set_size))
ar_forecasts <- rolling_windows %>% model(ARIMA(value ~ PDQ(0,0,0))) %>% forecast(h=forecast_horizon)
ar_forecasts <- (ar_forecasts %>% filter(.id<=no_horizons))$.mean

leakage_predictions <- c()
for(i in 1:test_set_size){
  step_forecast <- all_imf_predictions[[1]][i] + all_imf_predictions[[2]][i] + all_imf_predictions[[3]][i] + all_imf_predictions[[4]][i] + all_imf_predictions[[5]][i] + ar_forecasts[i]
  leakage_predictions <- c(leakage_predictions, step_forecast)
}

###### no leakage experiment
no_leakage_predictions <- c()
for (horizon in 1:no_horizons){
  data_train <- as.vector(myTS)$x[1:(length(as.vector(myTS)$x) - test_set_size + (horizon - 1) * forecast_horizon)]
  emd_output <- emd(data_train)

  # perform random forest modelling for the imfs
  models <- list()
  final_windows <- list()
  for (i in 1:emd_output$nimf){
    embedded_data <- data.frame(embed(emd_output$imf[, i], (input_size + 1))[,c((input_size + 1):1),drop=FALSE])
    final_windows[[i]] <- embedded_data[nrow(embedded_data), 2:ncol(embedded_data)]
    modRF <- randomForest(x=embedded_data[,-ncol(embedded_data)], y=embedded_data[,ncol(embedded_data)])
    models[[i]] <- modRF
  }

  # perform recursive forecasting for the imfs
  all_imf_predictions <- rep(0, forecast_horizon)
  for (imf in 1:emd_output$nimf){
    model <- models[[imf]]
    predictions <- c()
    final_window <- final_windows[[imf]]

    colnames(final_window) <- paste0("X", c(1:input_size))
    for (h in c(1:forecast_horizon)){
      pred <- predict(model, final_window)
      predictions <- c(predictions, pred)

      # update the final window
      final_window <- c(final_window[2:length(final_window)], pred)
      final_window <- t(as.matrix(final_window))
      colnames(final_window) <- paste0("X", c(1:input_size))
    }
    all_imf_predictions <- all_imf_predictions + predictions
  }

  # AR modelling for the residual
  residue <- as_tibble(emd_output$residue)
  residue <- residue %>% mutate(index=seq(as.Date("2015/01/01"), by = "day", length.out = nrow(residue)))
  residue <- residue %>% as_tsibble(index = index)

  ar_forecasts <- residue %>% model(ARIMA(value ~ PDQ(0,0,0))) %>% forecast(h=forecast_horizon)
  no_leakage_predictions <- c(no_leakage_predictions, (all_imf_predictions + ar_forecasts$.mean))
}

###### naive forecast
naive_predictions <- c()
actuals <- c()
for (i in 1:no_horizons){
  forecast_index <- length(as.vector(myTS)$x) - test_set_size + ((i - 1) * forecast_horizon)
  pred <- rep(as.vector(myTS)$x[forecast_index], each=forecast_horizon)
  naive_predictions <- c(naive_predictions, pred)
  actual <- as.vector(myTS)$x[(forecast_index + 1): (forecast_index + forecast_horizon)]
  actuals <- c(actuals, actual)
}

##### compute errors
MAE <- function(predictions, actuals, row=NULL){
  if (is.null(row)){
    mae <- mean(abs(predictions - actuals))
  }else if(row){
    mae <- rowMeans(abs(predictions - actuals))
  }else{
    mae <- colMeans(abs(predictions - actuals))
  }
  mae
}

RMSE <- function(predictions, actuals, row=NULL){
  if (is.null(row)){
    mae <- sqrt(mean((predictions - actuals) ^ 2))
  }else if(row){
    mae <- sqrt(rowMeans((predictions - actuals) ^ 2))
  }else{
    mae <- sqrt(colMeans((predictions - actuals) ^ 2))
  }
  mae
}

MAE(naive_predictions, actuals)
MAE(leakage_predictions, actuals)
MAE(no_leakage_predictions, actuals)

RMSE(naive_predictions, actuals)
RMSE(leakage_predictions, actuals)
RMSE(no_leakage_predictions, actuals)

###### plot the forecasts
plot(actuals, type="l", col="black", ylab="share price", xlab="time")
lines(naive_predictions, type = "l", col="red")
lines(leakage_predictions, type = "l", col="blue")
lines(no_leakage_predictions, type = "l", col="green")
legend("topleft", legend=c("Reference", "Naive", "Leakage", "No Leakage"), col=c("black", "red", "blue", "green"), lty=1)

###### check the statistical significance
# get the predictions for the 10 iterations
get_iterative_predictions <- function (predictions, horizon){
  matrix(predictions, nrow = horizon)
}

naive_mae <- MAE(get_iterative_predictions(naive_predictions, horizon = forecast_horizon),
    get_iterative_predictions(actuals, horizon = forecast_horizon), row=FALSE)
leakage_mae <- MAE(get_iterative_predictions(leakage_predictions, horizon = forecast_horizon),
    get_iterative_predictions(actuals, horizon = forecast_horizon), row=FALSE)
no_leakage_mae <- MAE(get_iterative_predictions(no_leakage_predictions, horizon = forecast_horizon),
    get_iterative_predictions(actuals, horizon = forecast_horizon), row=FALSE)

naive_rmse <- RMSE(get_iterative_predictions(naive_predictions, horizon = forecast_horizon),
    get_iterative_predictions(actuals, horizon = forecast_horizon), row=FALSE)
leakage_rmse <- RMSE(get_iterative_predictions(leakage_predictions, horizon = forecast_horizon),
    get_iterative_predictions(actuals, horizon = forecast_horizon), row=FALSE)
no_leakage_rmse <- RMSE(get_iterative_predictions(no_leakage_predictions, horizon = forecast_horizon),
    get_iterative_predictions(actuals, horizon = forecast_horizon), row=FALSE)

wilcox.test(no_leakage_rmse, naive_rmse, paired = TRUE)
wilcox.test(leakage_rmse, naive_rmse, paired = TRUE)

write.csv(naive_mae, "./results/STL_Decomp/naive_mae.csv", row.names = FALSE)
write.csv(leakage_mae, "./results/STL_Decomp/leakage_mae.csv", row.names = FALSE)
write.csv(no_leakage_mae, "./results/STL_Decomp/no_leakage_mae.csv", row.names = FALSE)

write.csv(naive_rmse, "./results/STL_Decomp/naive_rmse.csv", row.names = FALSE)
write.csv(leakage_rmse, "./results/STL_Decomp/leakage_rmse.csv", row.names = FALSE)
write.csv(no_leakage_rmse, "./results/STL_Decomp/no_leakage_rmse.csv", row.names = FALSE)