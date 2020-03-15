#' Convert a matrix of sampled trajectories to binned distributions for
#' short-term and/or seasonal targets.
#' 
#' @param trajectories matrix of simulated trajectories.  Each row is one
#'    simulated trajectory, each column is one time point.  Must contain all
#'    time points needed to calculate seasonal and short-term targets
#' @param h_max largest horizon for short-term targets
#' @param bins: named list of bin start and end points.  For example:
#'    list(
#'      "season peak incidence" = c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0),
#'      "season peak timing" = c(seq(from = 40, to = 53), seq(from = 1, to = 20)),
#'      "onset timing" = c(as.character(c(seq(from = 40, to = 53), seq(from = 1, to = 20))), "none")
#' @param season_start_ew: character specification of season start epidemic
#'    week, formatted as "2019-EW40"
#' @param season_end_ew: character specification of season end epidemic week,
#'    formatted as "2020-EW20"
#' @param cdc_report_ew: character specification of epidemic week corresponding
#'    to most recent cdc report, formatted as "2019-EW40"
#' 
#' @return data frame with columns:
#'    target: with values coming from names(bins)
#'    type: populated with "Bin"
#'    bin_start_incl: lower endpoints of bins
#'    bin_end_notincl: upper endpoints of bins
#'    value: proportion of trajectories falling in bin
#' 
#' @export
trajectories_to_binned_distributions <-
function(
  trajectories,
  h_max,
  bins,
  season_start_ew,
  season_end_ew,
  cdc_report_ew
)
{
  # set up globals
  date_seq <-date_start_and_end_to_date_seq(season_start_ew,season_end_ew)
  idx_of_current_time <- get_current_time_in_date_seq(cdc_report_ew,date_seq)
  
  short_term_results <- purrr::map_dfr(
    seq(idx_of_current_time+1,idx_of_current_time+h_max),
    function(h) {
      numeric_samples_to_binned_distribution(
        x = trajectories[, h],
        bins = bins) %>%
        dplyr::mutate(
          target = paste0(h-idx_of_current_time, " wk ahead"),
          type = "Bin"
        )
    }
  )
  
  season_peak_percentage <- numeric_samples_to_binned_distribution(
      x = rowMax(trajectories),
      bin = bins) %>%
    dplyr::mutate(
      target = "Peak Percentage",
      type = "Bin"
    )
  
  season_peak_week <- numeric_samples_to_binned_distribution(
      x = rowMaxWeek(trajectories),
      bin = seq(1,length(date_seq)+1)) %>%
    dplyr::mutate(
      target = "Peak Week",
      type = "Bin"
    )
  
  season_peak_week$bin <- date_seq[1:(length(date_seq))]

  submission_df <- rbind(short_term_results,season_peak_week,season_peak_percentage)
  submission_df$forecast_week <- cdc_report_ew
  
  return(submission_df)
}
