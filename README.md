This repository contains the experiments conducted for the publication 'Forecast Evaluation for Data Scientists: Common Pitfalls and Best Practices.'


## Long-Term Forecasting Experiments:
For these experiments, first the relevant input files need to be created using the `./ltf_experiments/ltf_experiments_files_creator.R file`. Then, the `./ltf_experiments/ECL_dhr_arima.R` and `./ltf_experiments/ETT_dhr_arima.R` files can be excuted to produce the forecasts from the DHR-ARIMA model. The forecasts are scaled using standard normalisation (using the same mean and std used in the Informer paper) to compute the final errors.

## Data Leakage Experiments:
The sample series used for this experiment is stored in `./data/simulated_exchange_rate/ts.csv` file. It is simulated using the script `./data_leakage_experiments/simulation.R` script.

## Exchange Rate Dataset Experiments:
The exchange rate dataset related experiments are in the script `./exchange_rate_experiments/exchange_rate_experiments.py` Python file. 

All the other plots and experiments used in the paper are available in the intro_plots.R script.

## Citing our Work:

When using this repository, please cite:

```
@misc{HEWAMALAGE2022,
  doi = {10.48550/ARXIV.2203.10716},
  url = {https://arxiv.org/abs/2203.10716},
  author = {Hewamalage, Hansika and Ackermann, Klaus and Bergmeir, Christoph},
  keywords = {Machine Learning (cs.LG), Methodology (stat.ME), FOS: Computer and information sciences, FOS: Computer and information sciences},
  title = {Forecast Evaluation for Data Scientists: Common Pitfalls and Best Practices},
  publisher = {arXiv},
  year = {2022},
  copyright = {arXiv.org perpetual, non-exclusive license}
}
```
