# TSForecasting
This repository contains the implementations related to the experiments of a set of publicly available datasets that are used in the time series forecasting research space.

The benchmark datasets are available at: https://zenodo.org/communities/forecasting.

All datasets contain univariate time series and they are availble in the sktime .ts format. The data can be loaded into the R environment in tsibble format [1] by following the example in "utils/data_loader.R". It uses a similar approach to the arff file loading method in R foreign package [2]. Download the .ts files as required from our Zenodo dataset repository and put them into "ts_data" folder.

The fixed horizon, rolling origin and feature calculation related experiments are there in the "experiments" folder. Please create a folder named "results" in the parent level and sub-folders as necessary before running the experiments. The outputs of the experiments will be stored into the sub-folders within the "results" folder as mentioned follows:

| Sub-folder Name               | Stored Output                  | 
|-------------------------------|:------------------------------:|
| forecasts                     | rolling origin forecasts       |
| errors                        | rolling origin errors          |
| execution_times               | rolling origin execution times |
| fixed_horizon_forecasts       | fixed horizon forecasts        |
| fixed_horizon_errors          | fixed horizon errors           |
| fixed_horizon_execution_times | fixed horizon execution times  |
| tsfeatures                    | tsfeatures                     |
| catch22_features              | catch22 features               |
| lambdas                       | boxcox lambdas                 |


# Citing Our Work
When using this repository, please cite:

```{r} 
@article{godahewa2020forecasting,
  title={Monash University, Time Series Forecasting Archive},
  author={Godahewa, Rakshitha and Bergmeir, Christoph and Webb, Geoffrey I.},
  year={2020}
}
```

## References
[1] Wang, E., Cook, D., Hyndman, R. J. (2020). A new tidy data structure to support exploration and modeling of temporal data. Journal of Computational and Graphical Statistics. doi:10.1080/10618600.2019.1695624.

[2] R Core Team (2018). foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', .... R package version 0.8-71. https://CRAN.R-project.org/package=foreign

