from gluonts.model.deepar import DeepAREstimator
from gluonts.model.n_beats import NBEATSEstimator
from gluonts.model.simple_feedforward import SimpleFeedForwardEstimator
from gluonts.model.transformer import TransformerEstimator
from gluonts.model.wavenet import WaveNetEstimator
from gluonts.dataset.common import ListDataset
from gluonts.dataset.field_names import FieldName
from gluonts.evaluation.backtest import make_evaluation_predictions
from datetime import datetime
import csv
import os
import subprocess
import numpy as np
import pandas as pd
import utils.data_loader as loader

BASE_DIR = "TSForecasting"

# The name of the column containing time series values after loading data from the .tsf file into a dataframe
VALUE_COL_NAME = "series_value"

# The name of the column containing timestamps after loading data from the .tsf file into a dataframe
TIME_COL_NAME = "start_timestamp"

# Seasonality values corresponding with the frequencies: minutely, 10_minutes, half_hourly, hourly, daily, weekly, monthly, quarterly and yearly
# Consider multiple seasonalities for frequencies less than daily
SEASONALITY_MAP = {
   "minutely": [1440, 10080, 525960],
   "10_minutes": [144, 1008, 52596],
   "half_hourly": [48, 336, 17532],
   "hourly": [24, 168, 8766],
   "daily": 7,
   "weekly": 365.25/7,
   "monthly": 12,
   "quarterly": 4,
   "yearly": 1
}

# Frequencies used by GluonTS framework
FREQUENCY_MAP = {
   "minutely": "1min",
   "10_minutes": "10min",
   "half_hourly": "30min",
   "hourly": "1H",
   "daily": "1D",
   "weekly": "1W",
   "monthly": "1M",
   "quarterly": "1Q",
   "yearly": "1Y"
}

# Parameters
# dataset_name - the name of the dataset
# lag - the number of past lags that should be used when predicting the next future value of time series
# input_file_name - name of the .tsf file corresponding with the dataset
# method - name of the forecasting method that you want to evaluate
# external_forecast_horizon - the required forecast horizon, if it is not available in the .tsf file
# integer_conversion - whether the forecasts should be rounded or not
def get_deep_nn_forecasts(dataset_name, lag, input_file_name, method, external_forecast_horizon = None, integer_conversion = False):
    print("Started loading " + dataset_name)

    df, frequency, forecast_horizon, contain_missing_values, contain_equal_length = loader.convert_tsf_to_dataframe(BASE_DIR + "/tsf_data/" + input_file_name, 'NaN', VALUE_COL_NAME)

    train_series_list = []
    test_series_list = []
    train_series_full_list = []
    test_series_full_list = []
    final_forecasts = []

    if frequency is not None:
        freq = FREQUENCY_MAP[frequency]
        seasonality = SEASONALITY_MAP[frequency]
    else:
        freq = "1Y"
        seasonality = 1

    if isinstance(seasonality, list):
        seasonality = min(seasonality) # Use to calculate MASE

    # If the forecast horizon is not given within the .tsf file, then it should be provided as a function input
    if forecast_horizon is None:
        if external_forecast_horizon is None:
            raise Exception("Please provide the required forecast horizon")
        else:
            forecast_horizon = external_forecast_horizon

    start_exec_time = datetime.now()

    for index, row in df.iterrows():
        if TIME_COL_NAME in df.columns:
            train_start_time = row[TIME_COL_NAME]
        else:
            train_start_time = datetime.strptime('1900-01-01 00-00-00', '%Y-%m-%d %H-%M-%S') # Adding a dummy timestamp, if the timestamps are not available in the dataset or consider_time is False

        series_data = row[VALUE_COL_NAME]

        # Creating training and test series. Test series will be only used during evaluation
        train_series_data = series_data[:len(series_data) - forecast_horizon]
        test_series_data = series_data[(len(series_data) - forecast_horizon) : len(series_data)]

        train_series_list.append(train_series_data)
        test_series_list.append(test_series_data)

        # We use full length training series to train the model as we do not tune hyperparameters
        train_series_full_list.append({
            FieldName.TARGET: train_series_data,
            FieldName.START: pd.Timestamp(train_start_time, freq=freq)
        })

        test_series_full_list.append({
            FieldName.TARGET: series_data,
            FieldName.START: pd.Timestamp(train_start_time, freq=freq)
        })

    train_ds = ListDataset(train_series_full_list, freq=freq)
    test_ds = ListDataset(test_series_full_list, freq=freq)

    if (method == "feed_forward"):
        estimator = SimpleFeedForwardEstimator(freq=freq,
                                               context_length=lag,
                                               prediction_length=forecast_horizon)
    elif(method == "deepar"):
        estimator = DeepAREstimator(freq=freq,
                                    context_length=lag,
                                    prediction_length=forecast_horizon)
    elif(method =="nbeats"):
        estimator = NBEATSEstimator(freq=freq,
                                    context_length=lag,
                                    prediction_length=forecast_horizon)
    elif (method == "wavenet"):
        estimator = WaveNetEstimator(freq=freq,
                                     prediction_length=forecast_horizon)
    elif (method == "transformer"):
        estimator = TransformerEstimator(freq=freq,
                                     context_length=lag,
                                     prediction_length=forecast_horizon)

    predictor = estimator.train(training_data=train_ds)

    forecast_it, ts_it = make_evaluation_predictions(dataset=test_ds, predictor=predictor, num_samples=100)

    # Time series predictions
    forecasts = list(forecast_it)

    # Get median (0.5 quantile) of the 100 sample forecasts as final point forecasts
    for f in forecasts:
        final_forecasts.append(f.median)

    if integer_conversion:
        final_forecasts = np.round(final_forecasts)

    if not os.path.exists(BASE_DIR + "/results/fixed_horizon_forecasts/"):
        os.makedirs(BASE_DIR + "/results/fixed_horizon_forecasts/")

    # write the forecasting results to a file
    file_name = dataset_name + "_" + method + "_lag_" + str(lag)
    forecast_file_path = BASE_DIR + "/results/fixed_horizon_forecasts/" + file_name + ".txt"

    with open(forecast_file_path, "w") as output:
        writer = csv.writer(output, lineterminator='\n')
        writer.writerows(final_forecasts)

    finish_exec_time = datetime.now()

    # Execution time
    exec_time = finish_exec_time - start_exec_time
    print(exec_time)

    if not os.path.exists(BASE_DIR + "/results/fixed_horizon_execution_times/"):
        os.makedirs(BASE_DIR + "/results/fixed_horizon_execution_times/")

    with open(BASE_DIR + "/results/fixed_horizon_execution_times/" + file_name + ".txt", "w") as output_time:
        output_time.write(str(exec_time))

    # Write training dataset and the actual results into separate files, which are then used for error calculations
    # We do not use the built-in evaluation method in GluonTS as some of the error measures we use are not implemented in that
    temp_dataset_path =  BASE_DIR + "/results/fixed_horizon_forecasts/" + dataset_name + "_dataset.txt"
    temp_results_path = BASE_DIR + "/results/fixed_horizon_forecasts/" + dataset_name + "_results.txt"

    with open(temp_dataset_path, "w") as output_dataset:
        writer = csv.writer(output_dataset, lineterminator='\n')
        writer.writerows(train_series_list)

    with open(temp_results_path, "w") as output_results:
        writer = csv.writer(output_results, lineterminator='\n')
        writer.writerows(test_series_list)

    if not os.path.exists(BASE_DIR + "/results/fixed_horizon_errors/"):
        os.makedirs(BASE_DIR + "/results/fixed_horizon_errors/")

    subprocess.call(["Rscript", "--vanilla", BASE_DIR + "/utils/error_calc_helper.R", BASE_DIR, forecast_file_path, temp_results_path, temp_dataset_path, str(seasonality), file_name ])

    # Remove intermediate files
    os.system("rm " + temp_dataset_path)
    os.system("rm " + temp_results_path)


# Experiments

# Feed-Forward Neural Network
get_deep_nn_forecasts("cif_2016_6", 15, "cif_6_dataset.tsf", "feed_forward", 6)
get_deep_nn_forecasts("cif_2016_12", 15, "cif_12_dataset.tsf", "feed_forward", 12)
get_deep_nn_forecasts("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "feed_forward")
get_deep_nn_forecasts("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m1_yearly", 2, "m1_yearly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m1_monthly", 15, "m1_monthly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m3_yearly", 2, "m3_yearly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m3_monthly", 15, "m3_monthly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m3_other", 2, "m3_other_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m4_monthly", 15, "m4_monthly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m4_weekly", 65, "m4_weekly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m4_daily", 9, "m4_daily_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("m4_hourly", 210, "m4_hourly_dataset.tsf", "feed_forward")
get_deep_nn_forecasts("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "feed_forward", 12, True)
get_deep_nn_forecasts("hospital", 15, "hospital_dataset.tsf", "feed_forward", 12, True)
get_deep_nn_forecasts("fred_md", 15, "fred_md_dataset.tsf", "feed_forward", 12)
get_deep_nn_forecasts("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "feed_forward", 8)
get_deep_nn_forecasts("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "feed_forward", 8)
get_deep_nn_forecasts("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "feed_forward", 8, True)
get_deep_nn_forecasts("solar_weekly", 6, "solar_weekly_dataset.tsf", "feed_forward", 5)
get_deep_nn_forecasts("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "feed_forward", 8, True)
get_deep_nn_forecasts("dominick", 10, "dominick_dataset.tsf", "feed_forward", 8)
get_deep_nn_forecasts("us_births", 9, "us_births_dataset.tsf", "feed_forward", 30, True)
get_deep_nn_forecasts("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "feed_forward", 30)
get_deep_nn_forecasts("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "feed_forward", 30, True)
get_deep_nn_forecasts("covid_deaths", 9, "covid_deaths_dataset.tsf", "feed_forward", 30, True)
get_deep_nn_forecasts("weather", 9, "weather_dataset.tsf", "feed_forward", 30)
get_deep_nn_forecasts("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "feed_forward", 168)
get_deep_nn_forecasts("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "feed_forward", 168, True)
get_deep_nn_forecasts("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "feed_forward", 1008)
get_deep_nn_forecasts("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "feed_forward", 168)
get_deep_nn_forecasts("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "feed_forward", 24, True)
get_deep_nn_forecasts("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "feed_forward", 30)
get_deep_nn_forecasts("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "feed_forward", 30, True)
get_deep_nn_forecasts("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "feed_forward", 336)
get_deep_nn_forecasts("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "feed_forward", 168)
get_deep_nn_forecasts("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "feed_forward", 30)


# Transformer
get_deep_nn_forecasts("cif_2016_6", 15, "cif_6_dataset.tsf", "transformer", 6)
get_deep_nn_forecasts("cif_2016_12", 15, "cif_12_dataset.tsf", "transformer", 12)
get_deep_nn_forecasts("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "transformer")
get_deep_nn_forecasts("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "transformer")
get_deep_nn_forecasts("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "transformer")
get_deep_nn_forecasts("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m1_yearly", 2, "m1_yearly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m1_monthly", 15, "m1_monthly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m3_yearly", 2, "m3_yearly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m3_monthly", 15, "m3_monthly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m3_other", 2, "m3_other_dataset.tsf", "transformer")
get_deep_nn_forecasts("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m4_monthly", 15, "m4_monthly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m4_weekly", 65, "m4_weekly_dataset.tsf", "transformer")
get_deep_nn_forecasts("m4_daily", 9, "m4_daily_dataset.tsf", "transformer")
get_deep_nn_forecasts("m4_hourly", 210, "m4_hourly_dataset.tsf", "transformer")
get_deep_nn_forecasts("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "transformer", 12, True)
get_deep_nn_forecasts("hospital", 15, "hospital_dataset.tsf", "transformer", 12, True)
get_deep_nn_forecasts("fred_md", 15, "fred_md_dataset.tsf", "transformer", 12)
get_deep_nn_forecasts("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "transformer", 8)
get_deep_nn_forecasts("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "transformer", 8)
get_deep_nn_forecasts("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "transformer", 8, True)
get_deep_nn_forecasts("solar_weekly", 6, "solar_weekly_dataset.tsf", "transformer", 5)
get_deep_nn_forecasts("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "transformer", 8, True)
get_deep_nn_forecasts("dominick", 10, "dominick_dataset.tsf", "transformer", 8)
get_deep_nn_forecasts("us_births", 9, "us_births_dataset.tsf", "transformer", 30, True)
get_deep_nn_forecasts("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "transformer", 30)
get_deep_nn_forecasts("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "transformer", 30, True)
get_deep_nn_forecasts("covid_deaths", 9, "covid_deaths_dataset.tsf", "transformer", 30, True)
get_deep_nn_forecasts("weather", 9, "weather_dataset.tsf", "transformer", 30)
get_deep_nn_forecasts("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "transformer", 168)
get_deep_nn_forecasts("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "transformer", 168, True)
get_deep_nn_forecasts("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "transformer", 1008)
get_deep_nn_forecasts("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "transformer", 168)
get_deep_nn_forecasts("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "transformer", 24, True)
get_deep_nn_forecasts("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "transformer", 30)
get_deep_nn_forecasts("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "transformer", 30, True)
get_deep_nn_forecasts("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "transformer", 336)
get_deep_nn_forecasts("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "transformer", 168)
get_deep_nn_forecasts("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "transformer", 30)


# DeepAR
get_deep_nn_forecasts("cif_2016_6", 15, "cif_6_dataset.tsf", "deepar", 6)
get_deep_nn_forecasts("cif_2016_12", 15, "cif_12_dataset.tsf", "deepar", 12)
get_deep_nn_forecasts("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "deepar")
get_deep_nn_forecasts("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "deepar")
get_deep_nn_forecasts("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "deepar")
get_deep_nn_forecasts("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m1_yearly", 2, "m1_yearly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m1_monthly", 15, "m1_monthly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m3_yearly", 2, "m3_yearly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m3_monthly", 15, "m3_monthly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m3_other", 2, "m3_other_dataset.tsf", "deepar")
get_deep_nn_forecasts("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m4_monthly", 15, "m4_monthly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m4_weekly", 65, "m4_weekly_dataset.tsf", "deepar")
get_deep_nn_forecasts("m4_daily", 9, "m4_daily_dataset.tsf", "deepar")
get_deep_nn_forecasts("m4_hourly", 210, "m4_hourly_dataset.tsf", "deepar")
get_deep_nn_forecasts("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "deepar", 12, True)
get_deep_nn_forecasts("hospital", 15, "hospital_dataset.tsf", "deepar", 12, True)
get_deep_nn_forecasts("fred_md", 15, "fred_md_dataset.tsf", "deepar", 12)
get_deep_nn_forecasts("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "deepar", 8)
get_deep_nn_forecasts("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "deepar", 8)
get_deep_nn_forecasts("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "deepar", 8, True)
get_deep_nn_forecasts("solar_weekly", 6, "solar_weekly_dataset.tsf", "deepar", 5)
get_deep_nn_forecasts("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "deepar", 8, True)
get_deep_nn_forecasts("dominick", 10, "dominick_dataset.tsf", "deepar", 8)
get_deep_nn_forecasts("us_births", 9, "us_births_dataset.tsf", "deepar", 30, True)
get_deep_nn_forecasts("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "deepar", 30)
get_deep_nn_forecasts("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "deepar", 30, True)
get_deep_nn_forecasts("covid_deaths", 9, "covid_deaths_dataset.tsf", "deepar", 30, True)
get_deep_nn_forecasts("weather", 9, "weather_dataset.tsf", "deepar", 30)
get_deep_nn_forecasts("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "deepar", 168)
get_deep_nn_forecasts("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "deepar", 168, True)
get_deep_nn_forecasts("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "deepar", 1008)
get_deep_nn_forecasts("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "deepar", 168)
get_deep_nn_forecasts("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "deepar", 24, True)
get_deep_nn_forecasts("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "deepar", 30)
get_deep_nn_forecasts("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "deepar", 30, True)
get_deep_nn_forecasts("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "deepar", 336)
get_deep_nn_forecasts("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "deepar", 168)
get_deep_nn_forecasts("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "deepar", 30)


# N-BEATS
get_deep_nn_forecasts("cif_2016_6", 15, "cif_6_dataset.tsf", "nbeats", 6)
get_deep_nn_forecasts("cif_2016_12", 15, "cif_12_dataset.tsf", "nbeats", 12)
get_deep_nn_forecasts("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "nbeats")
get_deep_nn_forecasts("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m1_yearly", 2, "m1_yearly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m1_monthly", 15, "m1_monthly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m3_yearly", 2, "m3_yearly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m3_monthly", 15, "m3_monthly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m3_other", 2, "m3_other_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m4_monthly", 15, "m4_monthly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m4_weekly", 65, "m4_weekly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m4_daily", 9, "m4_daily_dataset.tsf", "nbeats")
get_deep_nn_forecasts("m4_hourly", 210, "m4_hourly_dataset.tsf", "nbeats")
get_deep_nn_forecasts("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "nbeats", 12, True)
get_deep_nn_forecasts("hospital", 15, "hospital_dataset.tsf", "nbeats", 12, True)
get_deep_nn_forecasts("fred_md", 15, "fred_md_dataset.tsf", "nbeats", 12)
get_deep_nn_forecasts("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "nbeats", 8)
get_deep_nn_forecasts("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "nbeats", 8)
get_deep_nn_forecasts("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "nbeats", 8, True)
get_deep_nn_forecasts("solar_weekly", 6, "solar_weekly_dataset.tsf", "nbeats", 5)
get_deep_nn_forecasts("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "nbeats", 8, True)
get_deep_nn_forecasts("dominick", 10, "dominick_dataset.tsf", "nbeats", 8)
get_deep_nn_forecasts("us_births", 9, "us_births_dataset.tsf", "nbeats", 30, True)
get_deep_nn_forecasts("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "nbeats", 30)
get_deep_nn_forecasts("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "nbeats", 30, True)
get_deep_nn_forecasts("covid_deaths", 9, "covid_deaths_dataset.tsf", "nbeats", 30, True)
get_deep_nn_forecasts("weather", 9, "weather_dataset.tsf", "nbeats", 30)
get_deep_nn_forecasts("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "nbeats", 168)
get_deep_nn_forecasts("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "nbeats", 168, True)
get_deep_nn_forecasts("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "nbeats", 1008)
get_deep_nn_forecasts("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "nbeats", 168)
get_deep_nn_forecasts("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "nbeats", 24, True)
get_deep_nn_forecasts("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "nbeats", 30)
get_deep_nn_forecasts("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "nbeats", 30, True)
get_deep_nn_forecasts("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "nbeats", 336)
get_deep_nn_forecasts("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "nbeats", 168)
get_deep_nn_forecasts("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "nbeats", 30)


# WaveNet
get_deep_nn_forecasts("cif_2016_6", 15, "cif_6_dataset.tsf", "wavenet", 6)
get_deep_nn_forecasts("cif_2016_12", 15, "cif_12_dataset.tsf", "wavenet", 12)
get_deep_nn_forecasts("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "wavenet")
get_deep_nn_forecasts("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m1_yearly", 2, "m1_yearly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m1_monthly", 15, "m1_monthly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m3_yearly", 2, "m3_yearly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m3_monthly", 15, "m3_monthly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m3_other", 2, "m3_other_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m4_monthly", 15, "m4_monthly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m4_weekly", 65, "m4_weekly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m4_daily", 9, "m4_daily_dataset.tsf", "wavenet")
get_deep_nn_forecasts("m4_hourly", 210, "m4_hourly_dataset.tsf", "wavenet")
get_deep_nn_forecasts("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "wavenet", 12, True)
get_deep_nn_forecasts("hospital", 15, "hospital_dataset.tsf", "wavenet", 12, True)
get_deep_nn_forecasts("fred_md", 15, "fred_md_dataset.tsf", "wavenet", 12)
get_deep_nn_forecasts("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "wavenet", 8)
get_deep_nn_forecasts("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "wavenet", 8)
get_deep_nn_forecasts("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "wavenet", 8, True)
get_deep_nn_forecasts("solar_weekly", 6, "solar_weekly_dataset.tsf", "wavenet", 5)
get_deep_nn_forecasts("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "wavenet", 8, True)
get_deep_nn_forecasts("dominick", 10, "dominick_dataset.tsf", "wavenet", 8)
get_deep_nn_forecasts("us_births", 9, "us_births_dataset.tsf", "wavenet", 30, True)
get_deep_nn_forecasts("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "wavenet", 30)
get_deep_nn_forecasts("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "wavenet", 30, True)
get_deep_nn_forecasts("covid_deaths", 9, "covid_deaths_dataset.tsf", "wavenet", 30, True)
get_deep_nn_forecasts("weather", 9, "weather_dataset.tsf", "wavenet", 30)
get_deep_nn_forecasts("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "wavenet", 168)
get_deep_nn_forecasts("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "wavenet", 168, True)
get_deep_nn_forecasts("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "wavenet", 168)
get_deep_nn_forecasts("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "wavenet", 24, True)
get_deep_nn_forecasts("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "wavenet", 30)
get_deep_nn_forecasts("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "wavenet", 30, True)
get_deep_nn_forecasts("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "wavenet", 336)
get_deep_nn_forecasts("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "wavenet", 168)
get_deep_nn_forecasts("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "wavenet", 30)