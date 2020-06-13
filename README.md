# TSForecasting
This repository contains the implementations related to the experiments of a set of publicly available datasets that are used in the time series forecasting research space.

The benchmark datasets are available at: https://zenodo.org/communities/forecasting.

All datasets contain univariate time series and they are availble in the sktime .ts format. The data can be loaded into the R environment in tsibble format [1] by following the example in utils/data_loader.R. It uses a similar approach to the arff file loading method in R foreign package [2]. 

## References
[1] Wang, E., Cook, D., Hyndman, R. J. (2020). A new tidy data structure to support exploration and modeling of temporal data. Journal of Computational and Graphical Statistics. doi:10.1080/10618600.2019.1695624.

[2] R Core Team (2018). foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', .... R package version 0.8-71. https://CRAN.R-project.org/package=foreign

