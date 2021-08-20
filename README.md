# TSForecasting
This repository contains the implementations related to the experiments of a set of publicly available datasets that are used in the time series forecasting research space.

The benchmark datasets are available at: https://zenodo.org/communities/forecasting. For more details, please refer to our website: https://forecastingdata.org/ and paper: https://arxiv.org/abs/2105.06643.

All datasets contain univariate time series and they are availble in a new format that we name as .tsf, pioneered by the sktime .ts format. The data can be loaded into the R environment in tsibble format [1] by following the example in "utils/data_loader.R". It uses a similar approach to the arff file loading method in R foreign package [2]. The data can be loaded into the Python environment as a Pandas dataframe by following the example in "utils/data_loader.py". Download the .tsf files as required from our Zenodo dataset repository and put them into "tsf_data" folder.

Other implementations in this repository include: 
 - Developments of 6 local univariate forecasting models: ETS, ARIMA, Theta, TBATS, SES and DHR-ARIMA: [models/local_univariate_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/local_univariate_models.R)
 - A global pooled regression model: [models/global_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/global_models.R)
 - A global CatBoost model: [models/global_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/global_models.R)
 - A feed-forward neural network and 4 deep learning models: DeepAR, N-BEATS, WaveNet and Transformer: [experiments/deep_learning_experiments.py](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/deep_learning_experiments.py)
 - Feature calculations of all series: [experiments/feature_experiments.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/feature_experiments.R)
 - Calculations of 5 error measures to evaluate forecasts: [utils/error_calculator.R](https://github.com/rakshitha123/TSForecasting/blob/master/utils/error_calculator.R) 

Furthermore, we have implemented a wrapper to do fixed horizon forecasting mentioned in the paper to evaluate the 6 local models and global pooled regression and CatBoost models: [experiments/fixed_horizon.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon.R). It connects the pipeline of model evaluation including loading a dataset, training a model, forecasting from the model and calculating error measures where the full pipeline is executed for all local and global models using two single function calls (see the functions "do_fixed_horizon_local_forecasting" and "do_fixed_horizon_global_forecasting" in [experiments/fixed_horizon.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon.R)). We use these 2 wrapper functions with our model evaluation in our paper and the statements that we use to call these 2 functions with all datasets are available in [experiments/fixed_horizon.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon.R). 

A similar wrapper is implemented in Python for neural networks and deep learning experiments to execute the full pipeline of model evaluation using a single function call. For more details, please see the examples available at [experiments/deep_learning_experiments.py](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/deep_learning_experiments.py)

All experiments related to rolling origin forecasting and feature calculations are also there in the "experiments" folder. Please see the examples in the corresponding R scripts in the "experiments" folder for more details. The outputs of the experiments will be stored into the sub-folders within a folder named, "results" as mentioned follows:

| Sub-folder Name               | Stored Output                  | 
|-------------------------------|:------------------------------:|
| rolling_origin_forecasts      | rolling origin forecasts       |
| rolling_origin_errors         | rolling origin errors          |
| rolling_origin_execution_times| rolling origin execution times |
| fixed_horizon_forecasts       | fixed horizon forecasts        |
| fixed_horizon_errors          | fixed horizon errors           |
| fixed_horizon_execution_times | fixed horizon execution times  |
| tsfeatures                    | tsfeatures                     |
| catch22_features              | catch22 features               |
| lambdas                       | boxcox lambdas                 |


# Citing Our Work
When using this repository, please cite:

```{r} 
@misc{godahewa2021monash,
    author="Godahewa, Rakshitha and Bergmeir, Christoph and Webb, Geoffrey I. and Hyndman, Rob J. and Montero-Manso, Pablo",
    title="Monash Time Series Forecasting Archive",
    howpublished ="\url{https://arxiv.org/abs/2105.06643}",
    year="2021"
}
```

# References
[1] Wang, E., Cook, D., Hyndman, R. J. (2020). A new tidy data structure to support exploration and modeling of temporal data. Journal of Computational and Graphical Statistics. doi:10.1080/10618600.2019.1695624.

[2] R Core Team (2018). foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', .... R package version 0.8-71. https://CRAN.R-project.org/package=foreign

