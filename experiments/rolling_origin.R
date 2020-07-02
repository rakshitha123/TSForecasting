BASE_DIR <- "TSForecasting"

source(file.path(BASE_DIR, "utils", "data_loader.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "local_univariate_models.R", fsep = "/"))

# The name of the column containing time series values after loading data from the .ts file into a tsibble
VALUE_COL_NAME <- "series_value"

# The train-test split use for rolling origin evaluation. By default, it uses 80% of data for training (denoted by 0.8) and 20% of data for testing
TRAIN_SPLIT <- 0.8

# The number of forecasts provided in one iteration while performing the rolling origin evaluation
ROLLING_ORIGIN_FORECAST_HORIZON <- 1

# seasonality values corresponding with the frequencies: 10_minutes, half_hourly, hourly, daily, weekly, monthly, quarterly and yearly
# consider multiple seasonalities for frequencies less than daily
SEASONALITY_VALS <- list()
SEASONALITY_VALS[[1]] <- c(144, 1008, 52596)
SEASONALITY_VALS[[2]] <- c(48, 336, 17532)
SEASONALITY_VALS[[3]] <- c(24, 168, 8766)
SEASONALITY_VALS[[4]] <- 7
SEASONALITY_VALS[[5]] <- 365.25/7
SEASONALITY_VALS[[6]] <- 12 
SEASONALITY_VALS[[7]] <- 4
SEASONALITY_VALS[[8]] <- 1  

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
# input_file_name - name of the .ts file corresponding with the dataset
# key - the name of the attribute that should be used as the key when creating the tsibble
# index - the name of the time attribute that should be used as the index when creating the tsibble
# address_near_zero_instability - whether the dataset contains zeros, this will be used when calculating smape of forecasts
# integer_conversion - whether the forecasts should be rounded or not
do_rolling_origin_forecating <- function(dataset_name, method, input_file_name, key = NULL, index = NULL, address_near_zero_instability = FALSE, integer_conversion = FALSE){
  output_file_name <- paste0(dataset_name, "_", method, ".txt")
  
  # Loading data from the .ts file
  loaded_data <- convert_ts_to_tsibble(file.path(BASE_DIR, "ts_data", input_file_name, fsep = "/"), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  contain_equal_length <- loaded_data[[5]]
  
  if(is.null(frequency))
    frequency <- 1
  
  seasonality <- SEASONALITY_MAP[[frequency]]
  
  max_train_test_lengths <- find_train_test_lengths(dataset, contain_equal_length)
  
  all_serie_names <- unique(dataset$series_name)

  train_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[1]])
  actual_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[2]])
  forecast_matrix <- matrix(NA, nrow = length(all_serie_names), ncol = max_train_test_lengths[[2]])

  start_time <- Sys.time()
  
  for(s in seq_along(all_serie_names)){
    
    print(s)
    
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    
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
    
    write.table(t(c(all_serie_names[s], series_forecasts)), file.path(BASE_DIR, "results", "forecasts", output_file_name, fsep = "/"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE, append = TRUE)
    
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
  
  # Error calculations
  calculate_errors(forecast_matrix, actual_matrix, train_matrix, seasonality, file.path(BASE_DIR, "results", "errors", paste0(dataset_name, "_", method), fsep = "/"), address_near_zero_instability)
  
  # Execution time
  print(end_time - start_time)
  write(c(end_time - start_time), file = file.path(BASE_DIR, "results", "execution_times", output_file_name, fsep = "/"), append = FALSE)
}


# Example of usage
# do_rolling_origin_forecating("sample", "theta", "sample.ts", "series_name", "start_timestamp")
# do_rolling_origin_forecating("sample", "ses", "sample.ts", "series_name", "start_timestamp")
# do_rolling_origin_forecating("sample", "tbats", "sample.ts", "series_name", "start_timestamp")
# do_rolling_origin_forecating("sample", "dhr_arima", "sample.ts", "series_name", "start_timestamp")

