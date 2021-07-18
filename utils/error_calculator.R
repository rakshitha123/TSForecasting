library(smooth)

# Functions to calculate smape, msmape, mase, mae and rmse


# Function to calculate series wise smape values
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
calculate_smape <- function(forecasts, test_set){
  smape <- 2 * abs(forecasts - test_set) / (abs(forecasts) + abs(test_set))
  smape_per_series <- rowMeans(smape, na.rm = TRUE)
  smape_per_series
}


# Function to calculate series wise smape values
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
calculate_msmape <- function(forecasts, test_set){
  epsilon <- 0.1
  sum <- NULL
  comparator <- matrix((0.5 + epsilon), nrow = nrow(test_set), ncol = ncol(test_set))
  sum <- pmax(comparator, (abs(forecasts) + abs(test_set) + epsilon))
  smape <- 2 * abs(forecasts - test_set) / (sum)
  msmape_per_series <- rowMeans(smape, na.rm = TRUE)
  msmape_per_series
}


# Function to calculate series wise mase values
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
# training_set - a matrix containing the training series
# seasonality - frequency of the dataset, e.g. 12 for monthly
calculate_mase <- function(forecasts, test_set, training_set, seasonality){
  mase_per_series <- NULL
  
  for(k in 1 :nrow(forecasts)){
    te <- as.numeric(test_set[k,])
    te <- te[!is.na(te)]
    tr <- as.numeric(training_set[[k]])
    tr <- tr[!is.na(tr)]
    f <- as.numeric(forecasts[k,])
    f <- f[!is.na(f)]
    
    mase <- MASE(te, f, mean(abs(diff(tr, lag = min(seasonality), differences = 1))))
    
    if(is.na(mase))
      mase <- MASE(te, f, mean(abs(diff(tr, lag = 1, differences = 1))))
    
    mase_per_series[k] <- mase
  }
  
  mase_per_series <- mase_per_series[!is.infinite(mase_per_series) & !is.na(mase_per_series)]
  mase_per_series
}


# Function to calculate series wise mae values
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
calculate_mae <- function(forecasts, test_set){
  mae <- abs(forecasts-test_set)
  mae_per_series <- rowMeans(mae, na.rm=TRUE)
  
  mae_per_series
}


# Function to calculate series wise rmse values
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
calculate_rmse <- function(forecasts, test_set){
  squared_errors <- (forecasts-test_set)^2
  rmse_per_series <- sqrt(rowMeans(squared_errors, na.rm=TRUE))
  
  rmse_per_series
}


# Function to provide a summary of 4 error metrics: smape, mase, mae and rmse
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
# training_set - a matrix containing the training series
# seasonality - frequency of the dataset, e.g. 12 for monthly
# output_file_name - The prefix of error file names
# address_near_zero_instability - whether the forecasts or actual values can have zeros or not
calculate_errors <- function(forecasts, test_set, training_set, seasonality, output_file_name){
  #calculating smape
  smape_per_series <- calculate_smape(forecasts, test_set)
  
  #calculating msmape
  msmape_per_series <- calculate_msmape(forecasts, test_set)
  
  #calculating mase
  mase_per_series <- calculate_mase(forecasts, test_set, training_set, seasonality)
  
  #calculating mae
  mae_per_series <- calculate_mae(forecasts, test_set)
  
  #calculating rmse
  rmse_per_series <- calculate_rmse(forecasts, test_set)
  
  mean_smape <- paste0("Mean SMAPE: ", mean(smape_per_series))
  median_smape <- paste0("Median SMAPE: ", median(smape_per_series))
  mean_msmape <- paste0("Mean mSMAPE: ", mean(msmape_per_series))
  median_msmape <- paste0("Median mSMAPE: ", median(msmape_per_series))
  mean_mase <- paste0("Mean MASE: ", mean(mase_per_series))
  median_mase <- paste0("Median MASE: ", median(mase_per_series))
  mean_mae <- paste0("Mean MAE: ", mean(mae_per_series))
  median_mae <- paste0("Median MAE: ", median(mae_per_series))
  mean_rmse <- paste0("Mean RMSE: ", mean(rmse_per_series))
  median_rmse <- paste0("Median RMSE: ", median(rmse_per_series))
  
  print(mean_smape)
  print(median_smape)
  print(mean_msmape)
  print(median_msmape)
  print(mean_mase)
  print(median_mase)
  print(mean_mae)
  print(median_mae)
  print(mean_rmse)
  print(median_rmse)
  
  #writing error measures into files
  write.table(smape_per_series, paste0(output_file_name, "_smape.txt"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write.table(msmape_per_series, paste0(output_file_name, "_msmape.txt"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write.table(mase_per_series, paste0(output_file_name, "_mase.txt"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write.table(mae_per_series, paste0(output_file_name, "_mae.txt"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write.table(rmse_per_series, paste0(output_file_name, "_rmse.txt"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write(c(mean_smape, median_smape, mean_msmape, median_msmape, mean_mase, median_mase, mean_mae, median_mae, mean_rmse, median_rmse, "\n"), file = paste0(output_file_name, ".txt"), append = FALSE)
}
