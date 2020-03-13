trajectories_to_short_term_and_seasonal_binned_distributions <-
function(
  trajectories,
  h_max,
  bins,
  season_start,
  season_end,
  current_time,
  nsim,
  round_digits
)
{
  # set up globals
  date_seq <-date_start_and_end_to_date_seq(season_start,season_end)
  idx_of_current_time <- get_current_time_in_date_seq(current_time,date_seq)
    
  short_term_results <- purrr::map_dfr(
    seq(idx_of_current_time+1,idx_of_current_time+h_max),
    function(h) {
      numeric_samples_to_binned_distribution(
        x = trajectories[, h],
        bins = bins) %>%
        mutate(
          target = paste0(h-idx_of_current_time, " wk ahead"),
          type = "Bin"
        )
    }
  )
  
  
  season_peak_percentage <- purrr::map_dfr(
    1,
    function(traj_idx) {
      numeric_samples_to_binned_distribution(
        x = rowMax(trajectories),
        bins = bins) %>%
        mutate(
          target = "Peak Percentage",
          type = "Bin"
        )
    }
  )
  season_peak_week <- purrr::map_dfr(
    1,
    function(traj_idx) {
      numeric_samples_to_binned_distribution(
        x = rowMaxWeek(trajectories),
        bins = seq(1,length(date_seq))) %>%
        mutate(
          target = "Peak Week",
          type = "Bin"
        )
    }
  )
  season_peak_week$bin_start_incl <- date_seq[1:(length(date_seq)-1)]
  season_peak_week$bin_end_notincl <- date_seq[2:(length(date_seq))]
  submission_df <- rbind(short_term_results,season_peak_week,season_peak_percentage)
  submission_df$forecast_week <- current_time
  
  return (submission_df)
  
}
