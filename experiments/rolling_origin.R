BASE_DIR <- "TSForecasting"

source(file.path(BASE_DIR, "utils", "data_loader.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "local_univariate_models.R", fsep = "/"))

# The name of the column containing time series values after loading data from the .tsf file into a tsibble
VALUE_COL_NAME <- "series_value"

# The train-test split use for rolling origin evaluation. By default, it uses 80% of data for training (denoted by 0.8) and 20% of data for testing
TRAIN_SPLIT <- 0.8

# The number of forecasts provided in one iteration while performing the rolling origin evaluation
ROLLING_ORIGIN_FORECAST_HORIZON <- 1

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

for(f in seq_along(FREQUENCIES))
  SEASONALITY_MAP[[FREQUENCIES[f]]] <- SEASONALITY_VALS[[f]]

# This function finds the maximum lengths of traing and test sections that a dataset can have based on the length of its series
find_train_test_lengths <- function(data, contain_equal_length = TRUE, split = TRAIN_SPLIT){
  train_seres_length <- NULL
  test_series_length <- NULL
  
  all_serie_names <- unique(data$series_name)
  
  if(contain_equal_length){
    first_series_data <- data[data$series_name == as.character(all_serie_names[1]),]
    train_seres_length <- round(nrow(first_series_data)*split)
    test_series_length <- nrow(first_series_data) - train_seres_length
  }else{
    series_lengths <- NULL
    for(s in all_serie_names){
      series_data <- data[data$series_name == as.character(s),]
      series_lengths <- c(series_lengths, nrow(series_data))
    }
    train_seres_length <- round(max(series_lengths)*split)
    test_series_length <- max(series_lengths) - train_seres_length
  }
  
  list(train_seres_length, test_series_length)
}


# This function performs the rolling origin evaluation
#
# Parameters
# dataset_name - the name of the dataset
# method - the name of the forecasting method that should be used for rolling origin evaluation
#         current supporting forecasting methods are ets, theta, simple exponential smoothing, tbats, auto.arima and dynamic harmonic regression arima
#         you can also define new forecasting methods (probably in models/local_univariate_models.R) and link them within the function  
# input_file_name - name of the .tsf file corresponding with the dataset
# key - the name of the attribute that should be used as the key when creating the tsibble
# index - the name of the time attribute that should be used as the index when creating the tsibble
# integer_conversion - whether the forecasts should be rounded or not
do_rolling_origin_forecating <- function(dataset_name, method, input_file_name, key = NULL, index = NULL, integer_conversion = FALSE){
  
  print(paste0("Started ", dataset_name))
  
  output_file_name <- paste0(dataset_name, "_", method, ".txt")
  
  # Loading data from the .tsf file
  loaded_data <- convert_tsf_to_tsibble(file.path(BASE_DIR, "tsf_data", input_file_name, fsep = "/"), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  contain_equal_length <- loaded_data[[5]]
  
  
  if(!is.null(frequency))
    seasonality <- SEASONALITY_MAP[[frequency]]
  else
    seasonality <- 1
  
  
  max_train_test_lengths <- find_train_test_lengths(dataset, contain_equal_length)
  
  all_serie_names <- unique(dataset$series_name)
  
  train_list <- list()
  actual_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[2]])
  forecast_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[2]])
  
  start_time <- Sys.time()
  
  print("started Rolling Origin")
  
  dir.create(file.path(BASE_DIR, "results", "rolling_origin_forecasts", fsep = "/"), showWarnings = FALSE)
  
  for(s in seq_along(all_serie_names)){
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    
    if(nrow(series_data) == 2)
      split <- 1
    else
      split <- round(nrow(series_data) * TRAIN_SPLIT)
    
    train_series_data <- series_data[1:split,]
    test_series_data <- series_data[(split+1):nrow(series_data),]
    
    series_forecasts <- NULL
    
    if(!is.null(index)){
      start_date <- start(as.ts(train_series_data[, c(index, VALUE_COL_NAME)], frequency = max(seasonality)))
    }
    
    for(i in 1:nrow(test_series_data)){
      if(is.null(index))
        series <- forecast:::msts(train_series_data[[VALUE_COL_NAME]], seasonal.periods = seasonality)
      else
        series <- forecast:::msts(train_series_data[[VALUE_COL_NAME]], start = start_date, seasonal.periods = seasonality)
      
      if(method == "ets")
        f <- get_ets_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "theta")
        f <- get_theta_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "arima"){
        if(i == 1){
          result <- get_arima_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
          f <- result[[1]]
          model <- result[[2]]
        }else
          f <- get_arima_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON, model)
      }  
      else if(method == "ses")
        f <- get_ses_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "tbats")
        f <- get_tbats_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "dhr_arima"){
        if(i == 1){
          result <- get_dhr_arima_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
          f <- result[[1]]
          model <- result[[2]]
        }else
          f <- get_dhr_arima_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON, model)
      }  
      
      series_forecasts <- c(series_forecasts, f)
      
      train_series_data <- rbind(train_series_data, test_series_data[i,])
    }
    
    if(integer_conversion)
      series_forecasts <- round(series_forecasts)
    
    write.table(t(c(all_serie_names[s], series_forecasts)), file.path(BASE_DIR, "results", "rolling_origin_forecasts", output_file_name, fsep = "/"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE, append = TRUE)
    
    train_series_data <- train_series_data[[VALUE_COL_NAME]][1:split]
    test_series_data <- test_series_data[[VALUE_COL_NAME]]
    
    if(!contain_equal_length){
      remaining_test_length <- max_train_test_lengths[[2]] - length(test_series_data)
      test_series_data <- c(test_series_data, rep(NA, remaining_test_length))
      series_forecasts <- c(series_forecasts, rep(NA, remaining_test_length))
    }
    
    train_list[[s]] <- train_series_data
    forecast_matrix[s,] <- series_forecasts
    actual_matrix[s,] <- test_series_data
  }
  
  end_time <- Sys.time()
  
  print("Finished rolling origin")
  
  # Error calculations
  dir.create(file.path(BASE_DIR, "results", "rolling_origin_errors", fsep = "/"), showWarnings = FALSE)
  calculate_errors(forecast_matrix, actual_matrix, train_list, seasonality, file.path(BASE_DIR, "results", "rolling_origin_errors", paste0(dataset_name, "_", method), fsep = "/"))
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  dir.create(file.path(BASE_DIR, "results", "rolling_origin_execution_times", fsep = "/"), showWarnings = FALSE)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "rolling_origin_execution_times", output_file_name, fsep = "/"), append = FALSE)
}


# Example of usage
do_rolling_origin_forecating("sample", "theta", "sample.tsf", "series_name", "start_timestamp")
do_rolling_origin_forecating("sample", "ses", "sample.tsf", "series_name", "start_timestamp")
do_rolling_origin_forecating("sample", "tbats", "sample.tsf", "series_name", "start_timestamp")
do_rolling_origin_forecating("sample", "dhr_arima", "sample.tsf", "series_name", "start_timestamp")
