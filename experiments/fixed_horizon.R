BASE_DIR <- "TSForecasting"

source(file.path(BASE_DIR, "utils", "data_loader.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "global_model_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "local_univariate_models.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "global_models.R", fsep = "/"))

# The name of the column containing time series values after loading data from the .tsf file into a tsibble
VALUE_COL_NAME <- "series_value"

# Defining the required forecasting models based on frequency. 
# Current supporting forecasting methods are ets, theta, simple exponential smoothing, tbats, auto.arima and dynamic harmonic regression arima
# You can also define new forecasting methods (probably in models/local_univariate_models.R) and link them here as required
MODELS_HIGH_FREQ <- c("ses", "theta", "ets", "arima") # for yearly, quarterly, monthly, and daily datasets
MODELS_LOW_FREQ <- c("ses", "theta", "tbats", "dhr_arima") # for 4_seconds, minutely, 10_minutes, 15_minutes, half_hourly, hourly and weekly datasets

# Seasonality values corresponding with the frequencies: 4_seconds, minutely, 10_minutes, 15_minutes, half_hourly, hourly, daily, weekly, monthly, quarterly and yearly
# Consider multiple seasonalities for frequencies less than daily
SEASONALITY_VALS <- list()
SEASONALITY_VALS[[1]] <- c(21600, 151200, 7889400)
SEASONALITY_VALS[[2]] <- c(1440, 10080, 525960)
SEASONALITY_VALS[[3]] <- c(144, 1008, 52596)
SEASONALITY_VALS[[4]] <- c(96, 672, 35064)
SEASONALITY_VALS[[5]] <- c(48, 336, 17532)
SEASONALITY_VALS[[6]] <- c(24, 168, 8766)
SEASONALITY_VALS[[7]] <- 7
SEASONALITY_VALS[[8]] <- 365.25/7
SEASONALITY_VALS[[9]] <- 12 
SEASONALITY_VALS[[10]] <- 4
SEASONALITY_VALS[[11]] <- 1  

SEASONALITY_MAP <- list()

for(f in seq_along(FREQUENCIES)){
  SEASONALITY_MAP[[FREQUENCIES[f]]] <- SEASONALITY_VALS[[f]]
}


# This function performs the fixed horizon evaluation with local models
#
# Parameters
# dataset_name - the name of the dataset
# method - a list of forecasting models that needs to be evaluated
# input_file_name - name of the .tsf file corresponding with the dataset
# key - the name of the attribute that should be used as the key when creating the tsibble
# index - the name of the time attribute that should be used as the index when creating the tsibble
# external_forecast_horizon - the required forecast horizon, if it is not available in the .tsf file
# integer_conversion - whether the forecasts should be rounded or not

do_fixed_horizon_local_forecasting <- function(dataset_name, methods, input_file_name, key = NULL, index = NULL, external_forecast_horizon = NULL, integer_conversion = FALSE){
  
  for (method in methods) {
      file.remove(file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, ".txt")))
  }
  print(paste0("Started loading ", dataset_name))
  
  # Loading data from the .tsf file
  loaded_data <- convert_tsf_to_tsibble(file.path(BASE_DIR, "tsf_data", input_file_name, fsep = "/"), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  forecast_horizon <- loaded_data[[3]]
  
  
  if(!is.null(frequency))
    seasonality <- SEASONALITY_MAP[[frequency]]
  else
    seasonality <- 1
  
  
  # If the forecast horizon is not given within the .tsf file, then it should be provided as a function input
  if(is.null(forecast_horizon) & !("horizon" %in% colnames(dataset))){
    if(is.null(external_forecast_horizon))
      stop("Please provide the required forecast horizon")
    else
      forecast_horizon <- external_forecast_horizon
  }
  
  all_serie_names <- unique(dataset$series_name)
  
  train_series_list <- list()
  
  if(!is.null(forecast_horizon))
    actual_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = forecast_horizon)
  else
    actual_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max(as.numeric(dataset[["horizon"]])))
  
  
  start_time <- Sys.time()
  
  print("started Forecasting")
  
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_forecasts", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  
  for(s in seq_along(all_serie_names)){
    print(s)
    
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    
    if("horizon" %in% colnames(dataset))
      forecast_horizon <- as.numeric(unique(series_data[["horizon"]]))
    
    if(nrow(series_data) < forecast_horizon)
      forecast_horizon <- 1
    
    train_series_data <- series_data[1:(nrow(series_data) - forecast_horizon),]
    test_series_data <- series_data[(nrow(series_data) - forecast_horizon + 1):nrow(series_data),][[VALUE_COL_NAME]]
    
    if(forecast_horizon < ncol(actual_matrix))
      test_series_data <- c(test_series_data, rep(NA, (ncol(actual_matrix) - forecast_horizon)))
    
    train_series_list[[s]] <- train_series_data[[VALUE_COL_NAME]]
    actual_matrix[s,] <- test_series_data
    
    if(!is.null(index))
      start_date <- start(as.ts(train_series_data[, c(index, VALUE_COL_NAME)], frequency = max(seasonality)))
    
    if(is.null(index))
      series <- forecast:::msts(train_series_data[[VALUE_COL_NAME]], seasonal.periods = seasonality)
    else
      series <- forecast:::msts(train_series_data[[VALUE_COL_NAME]], start = start_date, seasonal.periods = seasonality)
    
    # Forecasting
    for(method in methods){
      current_method_forecasts <- eval(parse(text = paste0("get_", method, "_forecasts(series, forecast_horizon)")))
      
      if(typeof(current_method_forecasts) == "list")
        current_method_forecasts <- current_method_forecasts[[1]]
      
      current_method_forecasts[is.na(current_method_forecasts)] <- 0
      
      if(integer_conversion)
        current_method_forecasts <- round(current_method_forecasts)

      write.table(t(c(all_serie_names[s], current_method_forecasts)), file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, ".txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE, append = TRUE)
    }
  }
  
  end_time <- Sys.time()
  
  print("Finished Forecasting")
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_execution_times", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "fixed_horizon_execution_times", paste0(dataset_name, ".txt"), fsep = "/"), append = FALSE)
  
  # Error calculations
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_errors", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  
  for(method in methods){
    forecast_matrix <- read.csv(file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, ".txt"), fsep = "/"), header = F)
    forecast_matrix <- as.matrix(forecast_matrix[,-1])
    calculate_errors(forecast_matrix, actual_matrix, train_series_list, seasonality, file.path(BASE_DIR, "results", "fixed_horizon_errors", paste0(dataset_name, "_", method), fsep = "/"))
  }
}


# This function performs the fixed horizon evaluation with global models
# For our experiments, we only consider pooled regression under this category
#
# Parameters
# dataset_name - the name of the dataset
# lag - the number of past lags that should be used when predicting the next future value of time series
# input_file_name - name of the .tsf file corresponding with the dataset
# method - name of the global forecasting method that you want to evaluate
# key - the name of the attribute that should be used as the key when creating the tsibble
# index - the name of the time attribute that should be used as the index when creating the tsibble
# external_forecast_horizon - the required forecast horizon, if it is not available in the .tsf file
# integer_conversion - whether the forecasts should be rounded or not
do_fixed_horizon_global_forecasting <- function(dataset_name, lag, input_file_name, method, key = NULL, index = NULL, external_forecast_horizon = NULL, integer_conversion = FALSE){
  
  print(paste0("Started loading ", dataset_name))
  
  # Loading data from the .tsf file
  loaded_data <- convert_tsf_to_tsibble(file.path(BASE_DIR, "tsf_data", input_file_name, fsep = "/"), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  forecast_horizon <- loaded_data[[3]]
  
  
  if(!is.null(frequency))
    seasonality <- SEASONALITY_MAP[[frequency]]
  else
    seasonality <- 1
  
  
  if("horizon" %in% colnames(dataset))
    forecast_horizon <- max(as.numeric(dataset[["horizon"]]))
  
  # If the forecast horizon is not given within the .tsf file, then it should be provided as a function input  
  if(is.null(forecast_horizon)){
    if(is.null(external_forecast_horizon))
      stop("Please provide the required forecast horizon")
    else
      forecast_horizon <- external_forecast_horizon
  }
  
  all_serie_names <- unique(dataset$series_name)
  
  train_series_list <- list()
  
  actual_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = forecast_horizon)
  
  start_time <- Sys.time()
  
  print("started Forecasting")
  
  for(s in seq_along(all_serie_names)){
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    
    if("horizon" %in% colnames(dataset))
      series_forecast_horizon <- as.numeric(unique(series_data[["horizon"]]))
    else
      series_forecast_horizon <- forecast_horizon
    
    train_series_data <- series_data[1:(nrow(series_data) - series_forecast_horizon),]
    test_series_data <- series_data[(nrow(series_data) - series_forecast_horizon + 1):nrow(series_data),][[VALUE_COL_NAME]]
    
    if(series_forecast_horizon < ncol(actual_matrix))
      test_series_data <- c(test_series_data, rep(NA, (ncol(actual_matrix) - series_forecast_horizon)))
    
    train_series_list[[s]] <- train_series_data[[VALUE_COL_NAME]]
    actual_matrix[s,] <- test_series_data
  }
  
  # Forecasting
  forecast_matrix <- start_forecasting(train_series_list, lag, forecast_horizon, method)
  forecast_matrix[is.na(forecast_matrix)] <- 0
  
  if(integer_conversion)
    forecast_matrix <- round(forecast_matrix)
  
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_forecasts", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  
  for(s in seq_along(all_serie_names)){
    actual_series <- as.numeric(actual_matrix[s,])
    diff <- sum(is.na(actual_series))
    
    if(diff > 0)
      forecast_matrix[s, (ncol(forecast_matrix) - diff + 1) : ncol(forecast_matrix)] <- rep(NA, diff)
    
    write.table(t(c(all_serie_names[s], as.numeric(forecast_matrix[s,])[1:(length(actual_series) - diff)])), file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, "_lag_", lag, ".txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE, append = TRUE)
  }
  
  end_time <- Sys.time()
  
  print("Finished Forecasting")
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_execution_times", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "fixed_horizon_execution_times", paste0(dataset_name, "_", method, "_lag_", lag, ".txt"), fsep = "/"), append = FALSE)
  
  # Error calculations
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_errors", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  forecast_matrix <- as.matrix(forecast_matrix)
  calculate_errors(forecast_matrix, actual_matrix, train_series_list, seasonality, file.path(BASE_DIR, "results", "fixed_horizon_errors", paste0(dataset_name, "_", method, "_lag_", lag), fsep = "/"))
}



# Example of usage
#uncomment to try
#do_fixed_horizon_local_forecasting("sample", MODELS_LOW_FREQ, "sample.tsf", "series_name", "start_timestamp", 8)
#do_fixed_horizon_global_forecasting("sample", 65, "sample.tsf", "pooled_regression", "series_name", "start_timestamp", 8)



# Download the .tsf files from https://zenodo.org/communities/forecasting and put them into "tsf_data" folder, before trying the following examples

# Competition datasets - the same horizons expected in the competitons are used
# For global models, lag is chosen as (1.25 * seasonality)
# Due to high space and computational requirements, the lag is chosen as 50 for solar 10 minutes dataset and 10 (1.25 * forecast_horizon) for kaggle web traffic weekly dataset
# Due to short series length, lag is chosen as 10 for dominick dataset and 6 for solar weekly dataset (1.25 * forecast_horizon)
# For multi-seasonal datasets, the seasonality corresponding with the weekly frequency is chosen for lag calculation
# If it was not possible due to computational complexity or short series length, then we consider the seasonality corresponding with daily freqency for lag calculation

COMPUTE_FIXED_HORIZON_DSET = FALSE
if (COMPUTE_FIXED_HORIZON_DSET) {

do_fixed_horizon_local_forecasting("cif_2016", MODELS_HIGH_FREQ, "cif_2016_dataset.tsf")
do_fixed_horizon_local_forecasting("nn5_daily", MODELS_HIGH_FREQ, "nn5_daily_dataset_without_missing_values.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("tourism_yearly", MODELS_HIGH_FREQ, "tourism_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("tourism_quarterly", MODELS_HIGH_FREQ, "tourism_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("tourism_monthly", MODELS_HIGH_FREQ, "tourism_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m1_yearly", MODELS_HIGH_FREQ, "m1_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m1_quarterly", MODELS_HIGH_FREQ, "m1_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m1_monthly", MODELS_HIGH_FREQ, "m1_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_yearly", MODELS_HIGH_FREQ, "m3_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_quarterly", MODELS_HIGH_FREQ, "m3_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_monthly", MODELS_HIGH_FREQ, "m3_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_other", MODELS_HIGH_FREQ, "m3_other_dataset.tsf")
do_fixed_horizon_local_forecasting("m4_yearly", MODELS_HIGH_FREQ, "m4_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_quarterly", MODELS_HIGH_FREQ, "m4_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_monthly", MODELS_HIGH_FREQ, "m4_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_weekly", MODELS_LOW_FREQ, "m4_weekly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_daily", MODELS_HIGH_FREQ, "m4_daily_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_hourly", MODELS_LOW_FREQ, "m4_hourly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("kaggle_web_traffic_daily", MODELS_HIGH_FREQ, "kaggle_web_traffic_dataset_without_missing_values.tsf", "series_name", "start_timestamp", NULL, TRUE)

do_fixed_horizon_global_forecasting("cif_2016", 15, "cif_2016_dataset.tsf", "pooled_regression")
do_fixed_horizon_global_forecasting("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_yearly", 2, "m1_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_monthly", 15, "m1_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_yearly", 2, "m3_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_monthly", 15, "m3_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_other", 2, "m3_other_dataset.tsf", "pooled_regression")
do_fixed_horizon_global_forecasting("m4_yearly", 2, "m4_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_monthly", 15, "m4_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_weekly", 65, "m4_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_daily", 9, "m4_daily_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_hourly", 210, "m4_hourly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")

do_fixed_horizon_global_forecasting("cif_2016", 15, "cif_2016_dataset.tsf", "catboost")
do_fixed_horizon_global_forecasting("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_yearly", 2, "m1_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_monthly", 15, "m1_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_yearly", 2, "m3_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_monthly", 15, "m3_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_other", 2, "m3_other_dataset.tsf", "catboost")
do_fixed_horizon_global_forecasting("m4_yearly", 2, "m4_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_monthly", 15, "m4_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_weekly", 65, "m4_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_daily", 9, "m4_daily_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_hourly", 210, "m4_hourly_dataset.tsf", "catboost", "series_name", "start_timestamp")


# Monthly datasets - 1 year forecast horizon
do_fixed_horizon_local_forecasting("car_parts", MODELS_HIGH_FREQ, "car_parts_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_local_forecasting("hospital", MODELS_HIGH_FREQ, "hospital_dataset.tsf", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_local_forecasting("fred_md", MODELS_HIGH_FREQ, "fred_md_dataset.tsf", "series_name", "start_timestamp", 12)

do_fixed_horizon_global_forecasting("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("hospital", 15, "hospital_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("fred_md", 15, "fred_md_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 12)

do_fixed_horizon_global_forecasting("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("hospital", 15, "hospital_dataset.tsf", "catboost", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("fred_md", 15, "fred_md_dataset.tsf", "catboost", "series_name", "start_timestamp", 12)


# Weekly datasets - 5 - 8 weeks forecast horizon
do_fixed_horizon_local_forecasting("nn5_weekly", MODELS_LOW_FREQ, "nn5_weekly_dataset.tsf", "series_name", "start_timestamp", 8)
do_fixed_horizon_local_forecasting("traffic_weekly", MODELS_LOW_FREQ, "traffic_weekly_dataset.tsf", "series_name", "start_timestamp", 8)
do_fixed_horizon_local_forecasting("electricity_weekly", MODELS_LOW_FREQ, "electricity_weekly_dataset.tsf", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_local_forecasting("solar_weekly", MODELS_LOW_FREQ, "solar_weekly_dataset.tsf", "series_name", "start_timestamp", 5)
do_fixed_horizon_local_forecasting("kaggle_web_traffic_weekly", MODELS_LOW_FREQ, "kaggle_web_traffic_weekly_dataset.tsf", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_local_forecasting("dominick", MODELS_LOW_FREQ, "dominick_dataset.tsf", NULL, NULL, 8)

do_fixed_horizon_global_forecasting("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("solar_weekly", 6, "solar_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 5)
do_fixed_horizon_global_forecasting("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("dominick", 10, "dominick_dataset.tsf", "pooled_regression", NULL, NULL, 8)

do_fixed_horizon_global_forecasting("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("solar_weekly", 6, "solar_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 5)
do_fixed_horizon_global_forecasting("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("dominick", 10, "dominick_dataset.tsf", "catboost", NULL, NULL, 8)


# Daily datasets - 1 month forecast horizon (30 days)
do_fixed_horizon_local_forecasting("us_births", MODELS_HIGH_FREQ, "us_births_dataset.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("saugeen_river_flow", MODELS_HIGH_FREQ, "saugeenday_dataset.tsf", "series_name", "start_timestamp", 30)
do_fixed_horizon_local_forecasting("sunspot", MODELS_HIGH_FREQ, "sunspot_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("covid_deaths", MODELS_HIGH_FREQ, "covid_deaths_dataset.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("weather", MODELS_HIGH_FREQ, "weather_dataset.tsf", NULL, NULL, 30)
do_fixed_horizon_local_forecasting("bitcoin", MODELS_HIGH_FREQ, "bitcoin_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30)
do_fixed_horizon_local_forecasting("vehicle_trips", MODELS_HIGH_FREQ, "vehicle_trips_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("temperature_rain", MODELS_HIGH_FREQ, "temperature_rain_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30)

do_fixed_horizon_global_forecasting("us_births", 9, "us_births_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("covid_deaths", 9, "covid_deaths_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("weather", 9, "weather_dataset.tsf", "pooled_regression", NULL, NULL, 30)
do_fixed_horizon_global_forecasting("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30)

do_fixed_horizon_global_forecasting("us_births", 9, "us_births_dataset.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "catboost", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("covid_deaths", 9, "covid_deaths_dataset.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("weather", 9, "weather_dataset.tsf", "catboost", NULL, NULL, 30)
do_fixed_horizon_global_forecasting("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30)


# Hourly and other low frequency datasets - a horizon equal to the daily or weekly seasonality based on series length
do_fixed_horizon_local_forecasting("elecdemand", MODELS_LOW_FREQ, "elecdemand_dataset.tsf", "series_name", "start_timestamp", 336)
do_fixed_horizon_local_forecasting("traffic_hourly", MODELS_LOW_FREQ, "traffic_hourly_dataset.tsf", "series_name", "start_timestamp", 168)
do_fixed_horizon_local_forecasting("electricity_hourly", MODELS_LOW_FREQ, "electricity_hourly_dataset.tsf", "series_name", "start_timestamp", 168, TRUE)
do_fixed_horizon_local_forecasting("solar_10_minutes", MODELS_LOW_FREQ, "solar_10_minutes_dataset.tsf", "series_name", "start_timestamp", 1008)
do_fixed_horizon_local_forecasting("kdd_cup", MODELS_LOW_FREQ, "kdd_cup_2018_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 168)
do_fixed_horizon_local_forecasting("melbourne_pedestrian_counts", MODELS_LOW_FREQ, "pedestrian_counts_dataset.tsf", "series_name", "start_timestamp", 24, TRUE)
do_fixed_horizon_local_forecasting("aus_elecdemand", MODELS_LOW_FREQ, "australian_electricity_demand_dataset.tsf", "series_name", "start_timestamp", 336)
do_fixed_horizon_local_forecasting("rideshare", MODELS_LOW_FREQ, "rideshare_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 168)

do_fixed_horizon_global_forecasting("elecdemand", 420, "elecdemand_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 168, TRUE)
do_fixed_horizon_global_forecasting("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 1008)
do_fixed_horizon_global_forecasting("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 24, TRUE)
do_fixed_horizon_global_forecasting("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 168)

do_fixed_horizon_global_forecasting("elecdemand", 420, "elecdemand_dataset.tsf", "catboost", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "catboost", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "catboost", "series_name", "start_timestamp", 168, TRUE)
do_fixed_horizon_global_forecasting("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "catboost", "series_name", "start_timestamp", 1008)
do_fixed_horizon_global_forecasting("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "catboost", "series_name", "start_timestamp", 24, TRUE)
do_fixed_horizon_global_forecasting("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "catboost", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 168)

}






