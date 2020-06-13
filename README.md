# TSForecasting
This repository contains the implementations related to the experiments of a set of publicly available datasets that are used in the time series forecasting research space.

The benchmark datasets are available at: https://zenodo.org/communities/forecasting.

All datasets contain univariate time series and they are availble in the sktime .ts format. The data can be loaded to the R environment (tsibble format) by following the example in utils/data_loader.R. It uses a similar approach to the arff file loading method in R foreign package. 

## References
R Core Team (2018). foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', .... R package version 0.8-71. https://CRAN.R-project.org/package=foreign

