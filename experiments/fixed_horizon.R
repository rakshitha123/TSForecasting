BASE_DIR <- "TSForecasting"

source(file.path(BASE_DIR, "experiments", "fixed_horizon_functions.R", fsep = "/"))

# Download the .tsf files from https://zenodo.org/communities/forecasting and put them into "tsf_data" folder, before trying the following examples

# Competition datasets - the same horizons expected in the competitons are used
# For global models, lag is chosen as (1.25 * seasonality)
# Due to high space and computational requirements, the lag is chosen as 50 for solar 10 minutes dataset and 10 (1.25 * forecast_horizon) for kaggle web traffic weekly dataset
# Due to short series length, lag is chosen as 10 for dominick dataset and 6 for solar weekly dataset (1.25 * forecast_horizon)
# For multi-seasonal datasets, the seasonality corresponding with the weekly frequency is chosen for lag calculation
# If it was not possible due to computational complexity or short series length, then we consider the seasonality corresponding with daily freqency for lag calculation


do_fixed_horizon_local_forecasting("cif_2016", MODELS_HIGH_FREQ, "cif_2016_dataset.tsf")
do_fixed_horizon_local_forecasting("nn5_daily", MODELS_HIGH_FREQ, "nn5_daily_dataset_without_missing_values.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("tourism_yearly", MODELS_HIGH_FREQ, "tourism_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("tourism_quarterly", MODELS_HIGH_FREQ, "tourism_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("tourism_monthly", MODELS_HIGH_FREQ, "tourism_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m1_yearly", MODELS_HIGH_FREQ, "m1_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m1_quarterly", MODELS_HIGH_FREQ, "m1_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m1_monthly", MODELS_HIGH_FREQ, "m1_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_yearly", MODELS_HIGH_FREQ, "m3_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_quarterly", MODELS_HIGH_FREQ, "m3_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_monthly", MODELS_HIGH_FREQ, "m3_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m3_other", MODELS_HIGH_FREQ, "m3_other_dataset.tsf")
do_fixed_horizon_local_forecasting("m4_yearly", MODELS_HIGH_FREQ, "m4_yearly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_quarterly", MODELS_HIGH_FREQ, "m4_quarterly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_monthly", MODELS_HIGH_FREQ, "m4_monthly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_weekly", MODELS_LOW_FREQ, "m4_weekly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_daily", MODELS_HIGH_FREQ, "m4_daily_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("m4_hourly", MODELS_LOW_FREQ, "m4_hourly_dataset.tsf", "series_name", "start_timestamp")
do_fixed_horizon_local_forecasting("kaggle_web_traffic_daily", MODELS_HIGH_FREQ, "kaggle_web_traffic_dataset_without_missing_values.tsf", "series_name", "start_timestamp", NULL, TRUE)

do_fixed_horizon_global_forecasting("cif_2016", 15, "cif_2016_dataset.tsf", "pooled_regression")
do_fixed_horizon_global_forecasting("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_yearly", 2, "m1_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_monthly", 15, "m1_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_yearly", 2, "m3_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_monthly", 15, "m3_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_other", 2, "m3_other_dataset.tsf", "pooled_regression")
do_fixed_horizon_global_forecasting("m4_yearly", 2, "m4_yearly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_monthly", 15, "m4_monthly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_weekly", 65, "m4_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_daily", 9, "m4_daily_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_hourly", 210, "m4_hourly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp")

do_fixed_horizon_global_forecasting("cif_2016", 15, "cif_2016_dataset.tsf", "catboost")
do_fixed_horizon_global_forecasting("nn5_daily", 9, "nn5_daily_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_yearly", 2, "tourism_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_quarterly", 5, "tourism_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("tourism_monthly", 15, "tourism_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_yearly", 2, "m1_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_quarterly", 5, "m1_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m1_monthly", 15, "m1_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_yearly", 2, "m3_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_quarterly", 5, "m3_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_monthly", 15, "m3_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m3_other", 2, "m3_other_dataset.tsf", "catboost")
do_fixed_horizon_global_forecasting("m4_yearly", 2, "m4_yearly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_quarterly", 5, "m4_quarterly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_monthly", 15, "m4_monthly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_weekly", 65, "m4_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_daily", 9, "m4_daily_dataset.tsf", "catboost", "series_name", "start_timestamp")
do_fixed_horizon_global_forecasting("m4_hourly", 210, "m4_hourly_dataset.tsf", "catboost", "series_name", "start_timestamp")


# Monthly datasets - 1 year forecast horizon
do_fixed_horizon_local_forecasting("car_parts", MODELS_HIGH_FREQ, "car_parts_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_local_forecasting("hospital", MODELS_HIGH_FREQ, "hospital_dataset.tsf", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_local_forecasting("fred_md", MODELS_HIGH_FREQ, "fred_md_dataset.tsf", "series_name", "start_timestamp", 12)

do_fixed_horizon_global_forecasting("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("hospital", 15, "hospital_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("fred_md", 15, "fred_md_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 12)

do_fixed_horizon_global_forecasting("car_parts", 15, "car_parts_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("hospital", 15, "hospital_dataset.tsf", "catboost", "series_name", "start_timestamp", 12, TRUE)
do_fixed_horizon_global_forecasting("fred_md", 15, "fred_md_dataset.tsf", "catboost", "series_name", "start_timestamp", 12)


# Weekly datasets - 5 - 8 weeks forecast horizon
do_fixed_horizon_local_forecasting("nn5_weekly", MODELS_LOW_FREQ, "nn5_weekly_dataset.tsf", "series_name", "start_timestamp", 8)
do_fixed_horizon_local_forecasting("traffic_weekly", MODELS_LOW_FREQ, "traffic_weekly_dataset.tsf", "series_name", "start_timestamp", 8)
do_fixed_horizon_local_forecasting("electricity_weekly", MODELS_LOW_FREQ, "electricity_weekly_dataset.tsf", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_local_forecasting("solar_weekly", MODELS_LOW_FREQ, "solar_weekly_dataset.tsf", "series_name", "start_timestamp", 5)
do_fixed_horizon_local_forecasting("kaggle_web_traffic_weekly", MODELS_LOW_FREQ, "kaggle_web_traffic_weekly_dataset.tsf", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_local_forecasting("dominick", MODELS_LOW_FREQ, "dominick_dataset.tsf", NULL, NULL, 8)

do_fixed_horizon_global_forecasting("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("solar_weekly", 6, "solar_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 5)
do_fixed_horizon_global_forecasting("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("dominick", 10, "dominick_dataset.tsf", "pooled_regression", NULL, NULL, 8)

do_fixed_horizon_global_forecasting("nn5_weekly", 65, "nn5_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("traffic_weekly", 65, "traffic_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8)
do_fixed_horizon_global_forecasting("electricity_weekly", 65, "electricity_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("solar_weekly", 6, "solar_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 5)
do_fixed_horizon_global_forecasting("kaggle_web_traffic_weekly", 10, "kaggle_web_traffic_weekly_dataset.tsf", "catboost", "series_name", "start_timestamp", 8, TRUE)
do_fixed_horizon_global_forecasting("dominick", 10, "dominick_dataset.tsf", "catboost", NULL, NULL, 8)


# Daily datasets - 1 month forecast horizon (30 days)
do_fixed_horizon_local_forecasting("us_births", MODELS_HIGH_FREQ, "us_births_dataset.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("saugeen_river_flow", MODELS_HIGH_FREQ, "saugeenday_dataset.tsf", "series_name", "start_timestamp", 30)
do_fixed_horizon_local_forecasting("sunspot", MODELS_HIGH_FREQ, "sunspot_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("covid_deaths", MODELS_HIGH_FREQ, "covid_deaths_dataset.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("weather", MODELS_HIGH_FREQ, "weather_dataset.tsf", NULL, NULL, 30)
do_fixed_horizon_local_forecasting("bitcoin", MODELS_HIGH_FREQ, "bitcoin_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30)
do_fixed_horizon_local_forecasting("vehicle_trips", MODELS_HIGH_FREQ, "vehicle_trips_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_local_forecasting("temperature_rain", MODELS_HIGH_FREQ, "temperature_rain_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 30)

do_fixed_horizon_global_forecasting("us_births", 9, "us_births_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("covid_deaths", 9, "covid_deaths_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("weather", 9, "weather_dataset.tsf", "pooled_regression", NULL, NULL, 30)
do_fixed_horizon_global_forecasting("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 30)

do_fixed_horizon_global_forecasting("us_births", 9, "us_births_dataset.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("saugeen_river_flow", 9, "saugeenday_dataset.tsf", "catboost", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("sunspot", 9, "sunspot_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("covid_deaths", 9, "covid_deaths_dataset.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("weather", 9, "weather_dataset.tsf", "catboost", NULL, NULL, 30)
do_fixed_horizon_global_forecasting("bitcoin", 9, "bitcoin_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30)
do_fixed_horizon_global_forecasting("vehicle_trips", 9, "vehicle_trips_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30, TRUE)
do_fixed_horizon_global_forecasting("temperature_rain", 9, "temperature_rain_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 30)


# Hourly and other low frequency datasets - a horizon equal to the daily or weekly seasonality based on series length
do_fixed_horizon_local_forecasting("elecdemand", MODELS_LOW_FREQ, "elecdemand_dataset.tsf", "series_name", "start_timestamp", 336)
do_fixed_horizon_local_forecasting("traffic_hourly", MODELS_LOW_FREQ, "traffic_hourly_dataset.tsf", "series_name", "start_timestamp", 168)
do_fixed_horizon_local_forecasting("electricity_hourly", MODELS_LOW_FREQ, "electricity_hourly_dataset.tsf", "series_name", "start_timestamp", 168, TRUE)
do_fixed_horizon_local_forecasting("solar_10_minutes", MODELS_LOW_FREQ, "solar_10_minutes_dataset.tsf", "series_name", "start_timestamp", 1008)
do_fixed_horizon_local_forecasting("kdd_cup", MODELS_LOW_FREQ, "kdd_cup_2018_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 168)
do_fixed_horizon_local_forecasting("melbourne_pedestrian_counts", MODELS_LOW_FREQ, "pedestrian_counts_dataset.tsf", "series_name", "start_timestamp", 24, TRUE)
do_fixed_horizon_local_forecasting("aus_elecdemand", MODELS_LOW_FREQ, "australian_electricity_demand_dataset.tsf", "series_name", "start_timestamp", 336)
do_fixed_horizon_local_forecasting("rideshare", MODELS_LOW_FREQ, "rideshare_dataset_without_missing_values.tsf", "series_name", "start_timestamp", 168)

do_fixed_horizon_global_forecasting("elecdemand", 420, "elecdemand_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 168, TRUE)
do_fixed_horizon_global_forecasting("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 1008)
do_fixed_horizon_global_forecasting("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 24, TRUE)
do_fixed_horizon_global_forecasting("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "pooled_regression", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "pooled_regression", "series_name", "start_timestamp", 168)

do_fixed_horizon_global_forecasting("elecdemand", 420, "elecdemand_dataset.tsf", "catboost", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("traffic_hourly", 30, "traffic_hourly_dataset.tsf", "catboost", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("electricity_hourly", 30, "electricity_hourly_dataset.tsf", "catboost", "series_name", "start_timestamp", 168, TRUE)
do_fixed_horizon_global_forecasting("solar_10_minutes", 50, "solar_10_minutes_dataset.tsf", "catboost", "series_name", "start_timestamp", 1008)
do_fixed_horizon_global_forecasting("kdd_cup", 210, "kdd_cup_2018_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 168)
do_fixed_horizon_global_forecasting("melbourne_pedestrian_counts", 210, "pedestrian_counts_dataset.tsf", "catboost", "series_name", "start_timestamp", 24, TRUE)
do_fixed_horizon_global_forecasting("aus_elecdemand", 420, "australian_electricity_demand_dataset.tsf", "catboost", "series_name", "start_timestamp", 336)
do_fixed_horizon_global_forecasting("rideshare", 210, "rideshare_dataset_without_missing_values.tsf", "catboost", "series_name", "start_timestamp", 168)







