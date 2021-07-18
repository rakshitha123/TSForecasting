# Creating embedded matrix and final lags to train the global models for a given lag
create_input_matrix <- function(dataset, lag){
  embedded_series <- NULL
  final_lags <- NULL
  series_means <- NULL
  
  for (i in 1:length(dataset)) {
    print(i)
    time_series <- as.numeric(dataset[[i]])
    
    mean <- mean(time_series)
    
    # Mean normalisation
    if(mean == 0)
      mean <- 1 # Avoid division by zero
    
    time_series <- time_series / mean
    series_means <- c(series_means, mean)
    
    # Embed the series
    embedded <- embed(time_series, lag + 1)
    
    if (!is.null(embedded_series)) {
      embedded_series <- as.matrix(embedded_series)
    }
    embedded_series <- rbind(embedded_series, embedded)
    
    # Creating the test set
    if (!is.null(final_lags)) {
      final_lags <- as.matrix(final_lags)
    }
    
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
    
    final_lags <- rbind(final_lags, current_series_final_lags)
  }
  
  # Adding proper column names for embedded_series and final_lags
  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag + 1)] <- paste("Lag", 1:lag, sep = "")
  
  final_lags <- as.data.frame(final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
  
  list(embedded_series, final_lags, series_means)
}
