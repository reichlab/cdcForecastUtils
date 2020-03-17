#' Get the ILI baseline for a combination of region and season
#'
#' @param location a string, either "National", "Region k", or "Regionk" where
#'   k in {1, ..., 10}
#' @param year integer year, e.g. 2015
#'
#' @return ili baseline value
#'
#' @export
get_ili_baseline <- function(location, year = 2015) {
  reg_string <- cdcForecastUtils::get_standard_location_code(location)
  baseline_reg_strings <- cdcForecastUtils::get_standard_location_code(
    FluSight::past_baselines$location
  )
  
  idx <- which(
    baseline_reg_strings == reg_string &
      FluSight::past_baselines$year == year)
  
  if(length(idx) == 0) {
    stop(paste0("No baseline for location \"", location, "\"."))
  }
  
  reg_baseline <- FluSight::past_baselines[idx, "value"]
  
  return(reg_baseline)
}