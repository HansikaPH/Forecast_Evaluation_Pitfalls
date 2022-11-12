import numpy as np
import pandas as pd

# This script contains the error computation for the DHR_ARIMA models on the ETTh1 and ECL datasets

# compute mae and mse
def MAE(pred, true):
    return np.mean(np.abs(pred - true))

def MSE(pred, true):
    return np.mean((pred - true) ** 2)

### ETT data
scaled_trues = np.load(
    "./results/ETT/Informer/informer_ETTh1_ftS_sl720_ll336_pl720_dm512_nh8_el2_dl1_df2048_atprob_fc5_ebtimeF_dtTrue_mxTrue_Exp_0/true_scaled.npy")
ro_size = 2144
all_predictions = pd.DataFrame()
for window in range(1, (ro_size + 1)):
    predictions = pd.read_csv("./results/ETT/dhr_arima/ETTh1_window_" + str(window) + "_dhr_arima.csv")
    all_predictions[str(window)] = np.squeeze(predictions.values)

# perform scaling of the predictions to compute errors
mean = 17.1282617
std = 9.17649102

all_predictions_scaled = (all_predictions - mean)/std

# reshape into the shape of the true values
all_predictions_reshaped = np.transpose(all_predictions_scaled.values)
all_predictions_reshaped = np.reshape(all_predictions_reshaped, scaled_trues.shape)

# compute errors
MAE(all_predictions_reshaped, scaled_trues)
MSE(all_predictions_reshaped, scaled_trues)

### ECL data
scaled_trues = np.load(
    "./results/ECL/Informer/informer_ECL_ftS_sl960_ll336_pl960_dm512_nh8_el2_dl1_df2048_atprob_fc5_ebtimeF_dtTrue_mxTrue_Exp_0/true_scaled.npy")
ro_size = 4288
all_predictions = pd.DataFrame()
for window in range(1, (ro_size + 1)):
    predictions = pd.read_csv("./results/ECL/dhr_arima/ECL_window_" + str(window) + "_dhr_arima.csv")
    all_predictions[str(window)] = np.squeeze(predictions.values)

# perform scaling of the predictions to compute errors
mean = 3361.17570063
std = 554.42670238

all_predictions_scaled = (all_predictions - mean)/std

# reshape into the shape of the true values
all_predictions_reshaped = np.transpose(all_predictions_scaled.values)
all_predictions_reshaped = np.reshape(all_predictions_reshaped, scaled_trues.shape)

# compute errors
MAE(all_predictions_reshaped, scaled_trues)
MSE(all_predictions_reshaped, scaled_trues)