# Implementation of a pooled regression model

library(glmnet)

set.seed(1)


# Forecasting with different lags
start_forecasting <- function(dataset, lag, forecast_horizon){
  # Creating embedded matrix (for model training) and test set
  result <- create_input_matrix(dataset, lag)
  
  embedded_series <- result[[1]] # Embedded matrix
  final_lags <- result[[2]] # Test set
  series_means <- result[[3]] # Mean value of each series
  
  fit_model(embedded_series, lag, final_lags, forecast_horizon, series_means)
}


# Fit and forecast from a global model
fit_model <- function(fitting_data, lag, final_lags, forecast_horizon, series_means) {
  # Create the formula
  formula <- "y ~ "
  for(predictor in 2:ncol(fitting_data)){
    if(predictor != ncol(fitting_data)){
      formula <- paste0(formula, colnames(fitting_data)[predictor], " + ")
    }else{
      formula <- paste0(formula, colnames(fitting_data)[predictor])
    }
  }
  
  formula <- paste(formula, "+ 0", sep="")
  formula <- as.formula(formula)
  
  # Fit the pooled regression model
  model <- glm(formula = formula, data = fitting_data)
  
  # Do forecasting
  forec_recursive(lag, model, final_lags, forecast_horizon, series_means)
}

  
# Recursive forecasting of the series until a given horizon
forec_recursive <- function(lag, model, final_lags, forecast_horizon, series_means) {
  
  # This will store the predictions corresponding with each horizon
  predictions <- NULL
  
  for (i in 1:forecast_horizon){
    # Get predictions for the current horizon
    new_predictions <- predict.glm(object = model, newdata = as.data.frame(final_lags)) 
    
    # Adding the current forecasts to the final predictions matrix
    predictions <- cbind(predictions, new_predictions)
    
    # Updating the test set for the next horizon
    if(i < forecast_horizon){
      final_lags <- final_lags[-lag]
      final_lags <- cbind(new_predictions, final_lags)
      colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  # Renormalise the predictions
  true_predictions <- predictions * as.vector(series_means)
  true_predictions
}
