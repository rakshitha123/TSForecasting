source(file.path(BASE_DIR, "utils", "data_loader.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "global_model_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "local_univariate_models.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "global_models.R", fsep = "/"))

# The name of the column containing time series values after loading data from the .tsf file into a tsibble
VALUE_COL_NAME <- "series_value"

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
do_fixed_horizon_local_forecasting <- function(dataset_name, method, input_file_name, key = NULL, index = NULL, external_forecast_horizon = NULL, integer_conversion = FALSE){
  
  file.remove(file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, ".txt")))
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
    
    current_method_forecasts <- eval(parse(text = paste0("get_", method, "_forecasts(series, forecast_horizon)")))
    
    if(typeof(current_method_forecasts) == "list")
      current_method_forecasts <- current_method_forecasts[[1]]
    
    current_method_forecasts[is.na(current_method_forecasts)] <- 0
    
    if(integer_conversion)
      current_method_forecasts <- round(current_method_forecasts)
    
    write.table(t(c(all_serie_names[s], current_method_forecasts)), file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, ".txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE, append = TRUE)
  }
  
  end_time <- Sys.time()
  
  print("Finished Forecasting")
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_execution_times", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "fixed_horizon_execution_times", paste0(dataset_name, "_", method, ".txt"), fsep = "/"), append = FALSE)
  
  # Error calculations
  dir.create(file.path(BASE_DIR, "results", "fixed_horizon_errors", fsep = "/"), showWarnings = FALSE, recursive=TRUE)
  
  forecast_matrix <- read.csv(file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, ".txt"), fsep = "/"), header = F)
  forecast_matrix <- as.matrix(forecast_matrix[-1])
  calculate_errors(forecast_matrix, actual_matrix, train_series_list, seasonality, file.path(BASE_DIR, "results", "fixed_horizon_errors", paste0(dataset_name, "_", method), fsep = "/"))
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
  
  file.remove(file.path(BASE_DIR, "results", "fixed_horizon_forecasts", paste0(dataset_name, "_", method, "_lag_", lag, ".txt"), fsep = "/"))
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
# uncomment to try

# do_fixed_horizon_local_forecasting("sample", "ses, "sample.tsf", "series_name", "start_timestamp", 8)
# do_fixed_horizon_global_forecasting("sample", 65, "sample.tsf", "pooled_regression", "series_name", "start_timestamp", 8)
