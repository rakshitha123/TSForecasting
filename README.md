# TSForecasting
This repository contains the implementations related to the experiments of a set of publicly available datasets that are used in the time series forecasting research space.

The benchmark datasets are available at: https://zenodo.org/communities/forecasting. For more details, please refer to our website: https://forecastingdata.org/ and paper: https://openreview.net/pdf?id=wEc1mgAjU-.

All datasets contain univariate time series and they are available in a new format that we name as .tsf, pioneered by the sktime .ts format. The data can be loaded into the R environment in tsibble format [1] by following the example in [utils/data_loader.R](https://github.com/rakshitha123/TSForecasting/blob/master/utils/data_loader.R). It uses a similar approach to the arff file loading method in R foreign package [2]. The data can be loaded into the Python environment as a Pandas dataframe by following the example in [utils/data_loader.py](https://github.com/rakshitha123/TSForecasting/blob/master/utils/data_loader.py). Download the .tsf files as required from our Zenodo dataset repository and put them into "tsf_data" folder.

Other implementations in this repository include: 
 - Developments of 6 local univariate forecasting models: ETS, ARIMA, Theta, TBATS, SES and DHR-ARIMA: [models/local_univariate_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/local_univariate_models.R)
 - A global pooled regression model: [models/global_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/global_models.R)
 - A global CatBoost model: [models/global_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/global_models.R)
 - A feed-forward neural network and 4 deep learning models: DeepAR, N-BEATS, WaveNet and Transformer: [experiments/deep_learning_experiments.py](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/deep_learning_experiments.py)
 - Feature calculations of all series: [experiments/feature_functions.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/feature_functions.R) and [experiments/feature_experiments.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/feature_experiments.R)
 - Calculations of 5 error measures to evaluate forecasts: [utils/error_calculator.R](https://github.com/rakshitha123/TSForecasting/blob/master/utils/error_calculator.R) 

Furthermore, we have implemented a wrapper to do fixed horizon forecasting mentioned in the paper to evaluate the 6 local models and global pooled regression and CatBoost models: [experiments/fixed_horizon_functions.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon_functions.R). It connects the pipeline of model evaluation including loading a dataset, training a model, forecasting from the model and calculating error measures where the full pipeline is executed for all local and global models using two single function calls (see the functions "do_fixed_horizon_local_forecasting" and "do_fixed_horizon_global_forecasting" in [experiments/fixed_horizon_functions.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon_functions.R)). We use these 2 wrapper functions with our model evaluation in our paper and the statements that we use to call these 2 functions with all datasets are available in [experiments/fixed_horizon.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon.R). 

A similar wrapper is implemented in Python for neural networks and deep learning experiments to execute the full pipeline of model evaluation using a single function call. For more details, please see the examples available at [experiments/deep_learning_experiments.py](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/deep_learning_experiments.py).

All experiments related to rolling origin forecasting and feature calculations are also there in the "experiments" folder. Please see the examples in the corresponding R scripts in the "experiments" folder for more details. 

Furthermore, we have implemented a Notebook showing how all feature and forecasting experiments implemented in R can be executed in Python. This Notebook is available at [experiments/forecastingdata_python.ipynb](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/forecastingdata_python.ipynb).


The outputs of the experiments will be stored into the sub-folders within a folder named, "results" as mentioned follows:

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

# Integration of New Forecasting Models
We also provide a simple interface for you to add other statistical, machine learning and deep learning models which we have not implemented in this framework. Please follow the below steps if you want to integrate a new forecasting model. Once, you integrate a new forecasting model, you can send us a pull request, so that we can integrate your implementation to our framework.  


## Integration of New Statistical Models
If you want to integrate a new statistical forecasting model named "alpha" to our framework, please add a new function in [models/local_univariate_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/local_univariate_models.R) with the name, "get_alpha_forecasts" as follows:

```{r} 
get_alpha_forecasts <- function(time_series, forecast_horizon){
  # Write the function body here to return the forecasts
}
```

Then, write the experiments in [experiments/fixed_horizon.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon.R) with the new model name to obtain the forecasts and evaluate the model. An example of executing the alpha statistical model with the NN5 daily dataset is shown below.

```{r} 
do_fixed_horizon_local_forecasting("nn5_daily", "alpha", "nn5_daily_dataset_without_missing_values.tsf", "series_name", "start_timestamp")
```

## Integration of New Machine Learning Models
If you want to integrate a new machine learning model named "alpha" to our framework, please add new if statements in the functions "fit_model" and "forec_recursive" in  [models/global_models.R](https://github.com/rakshitha123/TSForecasting/blob/master/models/global_models.R) as follows:

```{r} 
fit_model <- function(fitting_data, lag, final_lags, forecast_horizon, series_means, method) {
  # ...
  
  if(method == "alpha"){
     # Write the code here to fit the model 
     model <- xxx
  }
  
  # ...
}


forec_recursive <- function(lag, model, final_lags, forecast_horizon, series_means, method){
  # ...
  
  for (i in 1:forecast_horizon){  
    if(method == "alpha"){
      # Write the code here to get the predictions using the trained model
      new_predictions <- xxx
    } 
  }
  
  # ...
}
```

Then, write the experiments in [experiments/fixed_horizon.R](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/fixed_horizon.R) with the new model name to obtain the forecasts and evaluate the model. An example of executing the alpha machine learning model with the NN5 daily dataset is shown below.

```{r} 
do_fixed_horizon_global_forecasting("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "alpha", "series_name", "start_timestamp")
```

## Integration of New Deep Learning Models
If you want to integrate a new deep learning model (GluonTS based) named "alpha" to our framework, please add a new if statement in the method "get_deep_nn_forecasts" in  [experiments/deep_learning_experiments.py](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/deep_learning_experiments.py) as follows:

```{r} 
def get_deep_nn_forecasts(dataset_name, lag, input_file_name, method, external_forecast_horizon = None, integer_conversion = False):
  # ...
  
  if (method == "alpha"):
    # Write the code here to fit the model 
    
  # ...  
```

Then, write the experiments in [experiments/deep_learning_experiments.py](https://github.com/rakshitha123/TSForecasting/blob/master/experiments/deep_learning_experiments.py) with the new model name to obtain the forecasts and evaluate the model. An example of executing the alpha deep learning model with the NN5 daily dataset is shown below.

```{r} 
get_deep_nn_forecasts("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "alpha")
```

## Evaluation of New Forecasting Models
The forecasts provided by the new models you integrate will also be automatically evaluated in the same way as our forecasting models and thus, the results of your forecasting models and our forecasting models are directly comparable. You can also send the evaluation results of your new models, if you would like to publish them in our [website](https://forecastingdata.org/).

# Software/Package Versions

The versions of the software and packages that are used to conduct the experiments are mentioned in the following table. Using the exact same versions is not mandatory, however, the reproducibility of the benchmark evaluation results may depend on that.

| Software/Package       | Version        | 
|------------------------|:--------------:|
| R                      |  4.0.2         |
| Python                 |  3.7.4         |
| forecast               |  8.12          |
| glmnet                 |  4.0.2         |
| catboost               |  0.24.1        |
| smooth                 |  2.6.0         |
| GluonTS                |  0.8.0         |

# Citing Our Work
When using this repository, please cite:

```{r} 
@InProceedings{godahewa2021monash,
    author = "Godahewa, Rakshitha and Bergmeir, Christoph and Webb, Geoffrey I. and Hyndman, Rob J. and Montero-Manso, Pablo",
    title = "Monash Time Series Forecasting Archive",
    booktitle = "Neural Information Processing Systems Track on Datasets and Benchmarks",
    year = "2021"
}
```

# References
[1] Wang, E., Cook, D., Hyndman, R. J. (2020). A new tidy data structure to support exploration and modeling of temporal data. Journal of Computational and Graphical Statistics. doi:10.1080/10618600.2019.1695624.

[2] R Core Team (2018). foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', .... R package version 0.8-71. https://CRAN.R-project.org/package=foreign

