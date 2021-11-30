# Implementations of a set of univariate forecasting models
#
# Each function takes 2 parameters
# time_series - a ts object representing the time series that should be used with model training
# forecast_horizon - expected forecast horizon
#
# If a model fails to provide forecasts, it will return snaive forecasts


# Calculate ets forecasts
get_ets_forecasts <- function(time_series, forecast_horizon){
  tryCatch(
    forecast(forecast:::ets(time_series), h = forecast_horizon)$mean
  ,error = function(e) {
    warning(e)
    get_snaive_forecasts(time_series, forecast_horizon)
  })
}


# Calculate simple exponential smoothing forecasts
get_ses_forecasts <- function(time_series, forecast_horizon){
  tryCatch(
    forecast(forecast:::ses(time_series, h = forecast_horizon))$mean
  , error = function(e) {   
    warning(e)
    get_snaive_forecasts(time_series, forecast_horizon)
  })
}


# Calculate theta forecasts
get_theta_forecasts <-function(time_series, forecast_horizon){
  tryCatch(
    forecast:::thetaf(y = time_series, h = forecast_horizon)$mean
  , error = function(e) {   
    warning(e)
    get_snaive_forecasts(time_series, forecast_horizon)
  })
}


# Calculate auto.arima forecasts
get_arima_forecasts <- function(time_series, forecast_horizon, model = NULL){
  if(is.null(model)){
    tryCatch({
      fit <- forecast:::auto.arima(time_series, lambda = 0)
    }, error = function(e) {
        tryCatch({
          fit <<- forecast:::auto.arima(time_series)
        }, error = function(e){
            fit <<- forecast:::auto.arima(time_series, seasonal = FALSE)
        })
    })
    
    tryCatch({
      f <- forecast:::forecast.Arima(fit, h = forecast_horizon)$mean
      list(f, fit)
    }, error = function(e) { 
        warning(e)
        f <- get_snaive_forecasts(time_series, forecast_horizon)
        list(f, fit)
    })
  }else{
    tryCatch(
      forecast(forecast:::Arima(time_series, model = model), h = forecast_horizon)$mean
    , error = function(e) {   
        warning(e)
        get_snaive_forecasts(time_series, forecast_horizon)
    })
  }
}


# Calculate tbats forecasts
get_tbats_forecasts <- function(time_series, forecast_horizon){
  tryCatch(
    forecast(forecast:::tbats(time_series), h = forecast_horizon)$mean
  , error = function(e) {   
    warning(e)
    get_snaive_forecasts(time_series, forecast_horizon)
  })
}


# Calculate dynamic harmonic regression arima forecasts
get_dhr_arima_forecasts <- function(time_series, forecast_horizon, model = NULL){
  if(is.null(model)){
    tryCatch({
      xreg <- forecast:::fourier(time_series, K = 1)
      model <- forecast:::auto.arima(time_series, xreg = xreg, seasonal = FALSE)
      xreg1 <- forecast:::fourier(time_series, K = 1, h = forecast_horizon)
      f <- forecast(model, xreg = xreg1)$mean
      list(f, model)
    }, error = function(e) {   
      warning(e)
      f <- get_snaive_forecasts(time_series, forecast_horizon)
      list(f, model)
    })
  }else{
    tryCatch({
      xreg <- forecast:::fourier(time_series, K = 1)
      xreg1 <- forecast:::fourier(time_series, K = 1, h = forecast_horizon)
      forecast(forecast:::Arima(time_series, model = model, xreg = xreg), xreg = xreg1)$mean
    }, error = function(e) {   
      warning(e)
      get_snaive_forecasts(time_series, forecast_horizon)
    })
  }
}


# Calculate snaive forecasts
get_snaive_forecasts <- function(time_series, forecast_horizon){
  forecast:::snaive(time_series, h = forecast_horizon)$mean
}
