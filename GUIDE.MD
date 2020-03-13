# To use package modelers need two functions
- Fit method: takes a time series of arbitrary length and fits the model to the time series
- Predict method: takes a fitted model and produces a sample matrix of nsim*h 

# Steps to use package

- Fit model to data
- Compute h using the utility `time_left_in_season <- get_time_left_in_season(current_date_in_cdc_format,season_end)`
- Produce a matrix of `nsim*time_left_in_season` using predict method called the trajectory matrix
- (Optional) Apply custom backfill model to observed data
- Compute time since start of season using `time_from_start_of_season <- get_time_from_start_of_season(season_start,current_date_in_cdc_format)`
- If backfill: bind together backfill matrix of `nsim*time_from_start_of_season` and trajectory matrix
- If no backfill: create a matrix of rep(observed_data,nsim) and cbind to trajectory matrix
- Call `trajectories_to_short_term_and_seasonal_binned_distributions` to produce the submission df