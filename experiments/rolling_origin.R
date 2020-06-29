BASE_DIR <- "TSForecasting/"

source(paste0(BASE_DIR, "utils/data_loader.R"))
source(paste0(BASE_DIR, "utils/error_calculator.R"))
source(paste0(BASE_DIR, "models/local_univariate_models.R"))

# The name of the column containing time series values after loading data from the .ts file into a tsibble
VALUE_COL_NAME <- "series_value"

# The train-test split use for rolling origin evaluation. By default, it uses 80% of data for training (denoted by 0.8) and 20% of data for testing
TRAIN_SPLIT <- 0.8

# The number of forecasts provided in one iteration while performing the rolling origin evaluation
ROLLING_ORIGIN_FORECAST_HORIZON <- 1

# seasonality values corresponding with the frequencies: 10_minutes, half_hourly, hourly, daily, weekly, monthly, quarterly and yearly
SEASONALITY_VALS <- c(144, 48, 24, 7, 365.25/7, 12, 4, 1)
SEASONALITY_MAP <- hashmap:::hashmap(FREQUENCIES, SEASONALITY_VALS)


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
# input_file_name - name of the .ts file corresponding with the dataset
# key - the name of the attribute that should be used as the key when creating the tsibble
# index - the name of the time attribute that should be used as the index when creating the tsibble
do_rolling_origin_forecating <- function(dataset_name, method, input_file_name, key = NULL, index = NULL){
  output_file_name <- paste0(dataset_name, "_", method, ".txt")
  
  loaded_data <- convert_ts_to_tsibble(paste0(BASE_DIR, "ts_data/", input_file_name), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  contain_equal_length <- loaded_data[[5]]
  seasonality <- SEASONALITY_MAP[[frequency]]
  
  max_train_test_lengths <- find_train_test_lengths(dataset, contain_equal_length)
  
  all_serie_names <- unique(dataset$series_name)

  train_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[1]])
  actual_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[2]])
  forecast_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[2]])

  start_time <- Sys.time()
  
  for(s in 1:length(all_serie_names)){
    
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    
    split <- round(nrow(series_data) * TRAIN_SPLIT)
    train_series_data <- series_data[1:split,]
    test_series_data <- series_data[(split+1):nrow(series_data),]
    
    series_forecasts <- NULL
    
    for(i in 1:nrow(test_series_data)){
      
      if(is.null(index))
        series <- ts(train_series_data[[VALUE_COL_NAME]], frequency = seasonality)
      else
        series <- as.ts(train_series_data[, c(index, VALUE_COL_NAME)], frequency = seasonality)
      
      if(method == "ets")
        f <- get_ets_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "theta")
        f <- get_theta_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "arima")
        f <- get_arima_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "ses")
        f <- get_ses_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "tbats")
        f <- get_tbats_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      else if(method == "dhr_arima")
        f <- get_dhr_arima_forecasts(series, ROLLING_ORIGIN_FORECAST_HORIZON)
      
      series_forecasts <- c(series_forecasts, f)
      
      train_series_data <- rbind(train_series_data, test_series_data[i,])
    }
    
    write.table(t(c(all_serie_names[s], series_forecasts)), paste0(BASE_DIR, "results/", output_file_name), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE, append = TRUE)
    
    train_series_data <- train_series_data[[VALUE_COL_NAME]][1:split]
    test_series_data <- test_series_data[[VALUE_COL_NAME]]
    
    if(!contain_equal_length){
      remaining_train_length <- max_train_test_lengths[[1]] - length(train_series_data)
      remaining_test_length <- max_train_test_lengths[[2]] - length(test_series_data)
      
      train_series_data <- c(train_series_data, rep(NA, remaining_train_length))
      test_series_data <- c(test_series_data, rep(NA, remaining_test_length))
      series_forecasts <- c(series_forecasts, rep(NA, remaining_test_length))
    }
    
    train_matrix[s,] <- train_series_data
    forecast_matrix[s,] <- series_forecasts
    actual_matrix[s,] <- test_series_data
  }
  
  end_time <- Sys.time()
  
  calculate_errors(forecast_matrix, actual_matrix, train_matrix, seasonality)
  
  # Execution time
  print(end_time - start_time)
}


# Example of usage
do_rolling_origin_forecating("sample", "theta", "sample.ts", "series_name", "start_timestamp")
do_rolling_origin_forecating("sample", "ses", "sample.ts", "series_name", "start_timestamp")
do_rolling_origin_forecating("sample", "tbats", "sample.ts", "series_name", "start_timestamp")
do_rolling_origin_forecating("sample", "dhr_arima", "sample.ts", "series_name", "start_timestamp")



