import numpy as np
import pandas as pd

# This script contains the naive forecast computation for the exchange rate dataset
horizons = [96, 192, 336, 720]

# compute mae and mse
def MAE(pred, true):
    return np.mean(np.abs(pred - true))

def MSE(pred, true):
    return np.mean((pred - true) ** 2)

metrics = pd.DataFrame()
for horizon in horizons:
    # read the scaled true values and the validation dataset
    folder = "Exchange_96_" + str(horizon) + "_Autoformer_custom_ftM_sl96_ll48_pl" + str(horizon) + "_dm512_nh8_el2_dl1_df2048_fc3_ebtimeF_dtTrue_Exp_0"
    scaled_trues = np.load("./results/Exchange_Rate/Autoformer/" + folder + "/true_scaled.npy")
    scaled_valids = np.load("./results/Exchange_Rate/Autoformer/" + folder + "/valid_scaled.npy")

    # seperate the last values of each windows to act as the naive forecast
    last_steps = np.row_stack((scaled_valids[-1, :], scaled_trues[:, -1,:]))
    last_steps = last_steps[:-1, :]

    # repeat the last steps for the whole window
    last_steps = np.expand_dims(last_steps, axis=1)
    naive = np.repeat(last_steps, horizon, axis=1)

    mae = MAE(naive, scaled_trues)
    mse = MSE(naive, scaled_trues)

    metrics = metrics.append({'Horizon': horizon, 'MAE': mae, 'MSE': mse}, ignore_index=True)

metrics['Horizon'] = metrics['Horizon'].astype(int)
metrics.to_csv("./results/Exchange_Rate/Naive/naive_forecast_metrics.csv", index=False)