library(tidyverse)
library(Rcatch22)


source(file.path(BASE_DIR, "utils", "data_loader.R", fsep = "/"))


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

for(f in seq_along(FREQUENCIES))
  SEASONALITY_MAP[[FREQUENCIES[f]]] <- SEASONALITY_VALS[[f]]

# Considered set of tsfeatures except mean, variance and stl_features
TSFEATURE_NAMES <- c( "max_kl_shift",
                      "max_level_shift",
                      "max_var_shift",
                      "acf_features",
                      "arch_stat",
                      "crossing_points",
                      "entropy",
                      "flat_spots",
                      "holt_parameters",
                      "hurst",
                      "lumpiness",
                      "nonlinearity",
                      "pacf_features",
                      "stability",
                      "unitroot_kpss",
                      "unitroot_pp"
)


# This function calculates tsfeatures, catch22 features and BoxCox lambda values
# Parameters
# dataset_name - the name of the dataset
# input_file_name - name of the .tsf file corresponding with the dataset
# key - the name of the attribute that should be used as the key when creating the tsibble
# index - the name of the time attribute that should be used as the index when creating the tsibble
# feature_type  - tsfeatures, catch22 or lambda
calculate_features <- function(dataset_name, input_file_name, key = NULL, index = NULL, feature_type = "tsfeatures"){
  
  print(paste0("Started feature calculation: ", dataset_name))
  
  # Defining output file name
  if(feature_type == "lambda")
    output_file_name <- paste0(dataset_name, "_lambdas.csv")
  else
    output_file_name <- paste0(dataset_name, "_features.csv")
  
  
  # Loading data from the .tsf file
  loaded_data <- convert_tsf_to_tsibble(file.path(BASE_DIR, "tsf_data", input_file_name, fsep = "/"), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  
  if(!is.null(frequency))
    seasonality <- SEASONALITY_MAP[[frequency]]
  else
    seasonality <- 1
  
  all_serie_names <- unique(dataset$series_name)
  tslist <- list()
  
  for(s in seq_along(all_serie_names)){
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    
    if(is.null(index))
      series <- forecast:::msts(series_data[[VALUE_COL_NAME]], seasonal.periods = seasonality)
    else{
      start_date <- start(as.ts(series_data[, c(index, VALUE_COL_NAME)], frequency = max(seasonality)))
      
      if(length(start_date) == 1){ # Prepararing the start date according to the format required by stl_features such as peak and trough
        start_date <- c(floor(start_date), floor((start_date - floor(start_date)) * max(seasonality)))
      }
      
      series <- forecast:::msts(series_data[[VALUE_COL_NAME]], start = start_date, seasonal.periods = seasonality, ts.frequency = floor(max(seasonality)))
    }
    
    tslist[[s]] <- series
  }
  
  
  for(i in 1:length(tslist)){
    print(i)
    
    features <- NULL
    
    if(feature_type == "tsfeatures"){ # Calculating tsfeatures
      features <- tsfeatures:::tsfeatures(tslist[[i]], c("mean","var"), scale = FALSE, na.rm = TRUE)
      
      for(f in TSFEATURE_NAMES){
        calculated_features <- tsfeatures:::tsfeatures(tslist[[i]], features = f)
        
        if(sum(is.na(calculated_features)) > 0){ # if the calculated features contain missing values, then consider freequency as 1
          calculated_features <- tsfeatures:::tsfeatures(ts(tslist[[i]], frequency = 1), features = f)
          
          if(sum(is.na(calculated_features)) > 0){ # Still if there are missing values, modify the parameters of the corresponding function
            if(f == "max_kl_shift" | f == "max_level_shift" | f == "max_var_shift")
              calculated_features <- tsfeatures:::tsfeatures(tslist[[i]], features = f, width = 1)
            else{
              if(f == "arch_stat")
                calculated_features <- tsfeatures:::tsfeatures(tslist[[i]], features = f, lag = 1)
            }
          }
        }
        
        features <- bind_cols(features, calculated_features)
      }
      
      # Calculating stl_features
      tryCatch( 
        seasonal_features <- tsfeatures:::tsfeatures(tslist[[i]],"stl_features", s.window = 'periodic', robust = TRUE)
        , error = function(e) {
          tryCatch({
            seasonal_features <<- tsfeatures:::tsfeatures(tslist[[i]],"stl_features")
          }, error = function(e) {
            seasonal_features <<- tsfeatures:::tsfeatures(ts(tslist[[i]], frequency = 1),"stl_features") # Ignoring seasonality
          })
        })
      
      features <- bind_cols(features, seasonal_features)
      
      if(i == 1){
        all_features <- matrix(NA, nrow = length(tslist), ncol = ncol(features)) # Creating a matrix to store the calculated features
        colnames(all_features) <- colnames(features)
      }else{
        if(ncol(all_features) != ncol(features)){ # The features common to all series will be considered finally
          common_features <- intersect(colnames(all_features), colnames(features))
          all_features <- all_features[,common_features]
          features <- features[, common_features]
        }
      }
      
      all_features[i,] <- as.numeric(features)
      
    }else if(feature_type == "catch22"){ # Calculating catch22 features
      
      features <- catch22_all(tslist[[i]])
      
      if(i == 1){
        all_features <- matrix(NA, nrow = length(tslist), ncol = 22) # Creating a matrix to store the calculated features
        colnames(all_features) <- features$names
      }
      
      all_features[i,] <- features$values 
    }else{
      if(i == 1) # Calculating BoxCox lambda values
        lambdas <- forecast:::BoxCox.lambda(tslist[[i]])
      else
        lambdas <- c(lambdas, forecast:::BoxCox.lambda(tslist[[i]]))
    }
  }
  
  # Writing the calculated features into a file
  if(feature_type == "tsfeatures"){
    dir.create(file.path(BASE_DIR, "results", "tsfeatures", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write.table(all_features, file.path(BASE_DIR, "results", "tsfeatures", output_file_name, fsep = "/"), row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
  }else if(feature_type == "catch22"){
    dir.create(file.path(BASE_DIR, "results", "catch22_features", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write.table(all_features, file.path(BASE_DIR, "results", "catch22_features", output_file_name, fsep = "/"), row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
  }else{
    dir.create(file.path(BASE_DIR, "results", "lambdas", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write.table(lambdas, file.path(BASE_DIR, "results", "lambdas", output_file_name, fsep = "/"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  }
}



# Example Usage
#uncomment to try
## tsfeatures
#calculate_features("sample", "sample.tsf", "series_name", "start_timestamp")

## catch22 features
#calculate_features("sample", "sample.tsf", "series_name", "start_timestamp", "catch22")

## BoxCox lambdas
#calculate_features("sample", "sample.tsf", "series_name", "start_timestamp", "lambdas")