import numpy as np
import pandas as pd

# compute mae and mse
def MAE(pred, true):
    return np.mean(np.abs(pred - true))

def MSE(pred, true):
    return np.mean((pred - true) ** 2)

#############################################
######## Informer Paper
### ETT dataset - DHR
scaled_trues = np.load(
    "./Informer/results/informer_ETTh1_ftS_sl720_ll336_pl720_dm512_nh8_el2_dl1_df2048_atprob_fc5_ebtimeF_dtTrue_mxTrue_Exp_4/scaled_true.npy")

all_predictions = pd.DataFrame()
for window in range(1, 2145):
    predictions = pd.read_csv("./results/ETT/H_720/ETTh1_window_" + str(window) + ".csv")
    all_predictions[str(window)] = np.squeeze(predictions.values)

# perform scaling of the predictions
mean = 17.1282617
std = 9.17649102

all_predictions_scaled = (all_predictions - mean)/std

# reshape into the shape of the true values
all_predictions_reshaped = np.transpose(all_predictions_scaled.values)
all_predictions_reshaped = np.reshape(all_predictions_reshaped, scaled_trues.shape)

# compute errors
MAE(all_predictions_reshaped, scaled_trues)
MSE(all_predictions_reshaped, scaled_trues)

### ETT dataset - DHR-ARIMA
scaled_trues = np.load(
    "./Informer/results/informer_ETTh1_ftS_sl720_ll336_pl720_dm512_nh8_el2_dl1_df2048_atprob_fc5_ebtimeF_dtTrue_mxTrue_Exp_4/scaled_true.npy")

all_predictions = pd.DataFrame()
for window in range(1, 2145):
    predictions = pd.read_csv("./results/ETT/H_720/dhr_arima/ETTh1_window_" + str(window) + "dhr_arima.csv")
    all_predictions[str(window)] = np.squeeze(predictions.values)

# perform scaling of the predictions
mean = 17.1282617
std = 9.17649102

all_predictions_scaled = (all_predictions - mean)/std

# reshape into the shape of the true values
all_predictions_reshaped = np.transpose(all_predictions_scaled.values)
all_predictions_reshaped = np.reshape(all_predictions_reshaped, scaled_trues.shape)

# compute errors
MAE(all_predictions_reshaped, scaled_trues)
MSE(all_predictions_reshaped, scaled_trues)

### ECL dataset
scaled_trues = np.load(
    "./Informer/results/informer_ECL_ftS_sl960_ll336_pl960_dm512_nh8_el2_dl1_df2048_atprob_fc5_ebtimeF_dtTrue_mxTrue_Exp_0/true.npy")

all_predictions = pd.DataFrame()
for window in range(1, 4289):
    predictions = pd.read_csv("./results/ECL/H_960/ECL_window_" + str(window) + "dhr_arima.csv")
    all_predictions[str(window)] = np.squeeze(predictions.values)

# perform scaling of the predictions
mean = 3361.17570063
std = 554.42670238

all_predictions_scaled = (all_predictions - mean)/std

# reshape into the shape of the true values
all_predictions_reshaped = np.transpose(all_predictions_scaled.values)
all_predictions_reshaped = np.reshape(all_predictions_reshaped, scaled_trues.shape)

# compute errors
MAE(all_predictions_reshaped, scaled_trues)
MSE(all_predictions_reshaped, scaled_trues)

######################## STL Decomp
errors = pd.DataFrame(columns=["client", "MSE_Leakage", "MSE_No_Leakage"])
test_data_length = 24 * 7 * 3
data = pd.read_csv("./Informer/data/ECL.csv")
data = data.iloc[17544:20424, :]
for file_number in range(0, 100):
    pred_leakage = pd.read_csv("./results/STL_Decomp/data_leakage_3/predictions_" + str(file_number) + ".csv")
    pred_no_leakage = pd.read_csv("./results/STL_Decomp/no_data_leakage_3/predictions_" + str(file_number) + ".csv")
    trues = data.iloc[:, (file_number + 1)]
    trues = trues.iloc[-test_data_length:]

    mse_leakage = MSE(pred_leakage.values, trues.values)
    mse_no_leakage = MSE(pred_no_leakage.values, trues.values)

    errors = errors.append({"client": file_number, "MSE_Leakage": mse_leakage, "MSE_No_Leakage": mse_no_leakage}, ignore_index=True)

print("hello")
errors["check"] = errors.MSE_Leakage < errors.MSE_No_Leakage

from scipy.stats import ranksums
ranksums(x=errors.MSE_Leakage, y=errors.MSE_No_Leakage)
