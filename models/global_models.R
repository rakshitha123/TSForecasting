# Implementation of global models: pooled regression and CatBoost

library(glmnet)

failed_loading_catboost = FALSE
tryCatch(library(catboost), error = function(err) {failed_loading_catboost<<-TRUE})

set.seed(1)


# Forecasting with different lags
start_forecasting <- function(dataset, lag, forecast_horizon, method){
  # Creating embedded matrix (for model training) and test set
  result <- create_input_matrix(dataset, lag)
  
  embedded_series <- result[[1]] # Embedded matrix
  final_lags <- result[[2]] # Test set
  series_means <- result[[3]] # Mean value of each series
  
  fit_model(embedded_series, lag, final_lags, forecast_horizon, series_means, method)
}


# Fit and forecast from a global model
fit_model <- function(fitting_data, lag, final_lags, forecast_horizon, series_means, method) {
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
  
  # Fit global models
  if(method == "pooled_regression"){
    # Fit the pooled regression model
    model <- glm(formula = formula, data = fitting_data)
  }else if(method == "catboost"){
    if (failed_loading_catboost) stop("Error when loading catboost, cannot run global model based on catboost")
    # Fit the CatBoost model
    train_pool <- catboost.load_pool(data = as.matrix(fitting_data[-1]), label = as.matrix(fitting_data[,1]))
    model <- catboost.train(train_pool)
  }
  
  # Do forecasting
  forec_recursive(lag, model, final_lags, forecast_horizon, series_means, method)
}

  
# Recursive forecasting of the series until a given horizon
forec_recursive <- function(lag, model, final_lags, forecast_horizon, series_means, method){
  
  # This will store the predictions corresponding with each horizon
  predictions <- NULL
  
  for (i in 1:forecast_horizon){
    # Get predictions for the current horizon
    if(method == "pooled_regression")
      new_predictions <- predict.glm(object = model, newdata = as.data.frame(final_lags)) 
    else if(method == "catboost"){
      catboost_final_lags <- catboost.load_pool(final_lags)
      new_predictions <- catboost.predict(model, catboost_final_lags)
    }
    
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
