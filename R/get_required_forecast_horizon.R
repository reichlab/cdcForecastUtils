#' How many weeks ahead are required to forecast specified targets
#' 
#' @param targets: character vector specifying targets to compute. May include:
#'    "wk ahead", "Below baseline for 3 weeks", "First week below baseline",
#'    "Peak height", "Peak week"
#' @param h_max: horizon for week ahead forecasts
#' @param season_end_ew: character specification of season end epidemic week,
#'    formatted as "2020-EW20"
#' @param cdc_report_ew: character specification of epidemic week corresponding
#'    to most recent cdc report, formatted as "2019-EW40"
#' 
#' @export
get_required_forecast_horizon <- function(
  targets,
  h_max = 0,
  season_end_ew,
  cdc_report_ew
) {
  # targets in standardized form
  targets <- standardize_targets(targets)
  
  # sequence of ews covering rest of season
  date_seq <- date_start_and_end_to_date_seq(cdc_report_ew, season_end_ew)[-1]
  
  # minimum length as determined by peak targets: all remaining weeks in season
  peak_targets <- c("Peak height", "Peak week")
  if(any(peak_targets %in% targets)) {
    min_length_peak <- length(date_seq)
  } else {
    min_length_peak <- 0
  }
  
  # minimum length as determined by baseline targets: all remaining weeks in season + 2
  baseline_targets <- c("Below baseline for 3 weeks", "First week below baseline")
  if(any(baseline_targets %in% targets)) {
    min_length_baseline <- length(date_seq) + 2
  } else {
    min_length_baseline <- 0
  }
  
  # minimum length as determined by week ahead targets: h_max
  if("wk ahead" %in% targets) {
    min_length_wk_ahead <- h_max
  } else {
    min_length_wk_ahead <- 0
  }
  
  min_length <- max(min_length_peak, min_length_baseline, min_length_wk_ahead)
  
  return(min_length)
}
