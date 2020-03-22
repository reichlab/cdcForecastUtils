#' Save submitted targets/locations of a file
#'
#' This function will retrieve information from an entry file about what 
#' locations and targets were submitted for a particular forecast due date.
#'
#' @param sanitized_file A csv entry file path
#' @return A data.frame of a set of submitted targets/locations of a model
#' @import dplyr
#' @export
get_targets_regions<- function(sanitized_file) {
  entry <- read.csv(sanitized_file, 
                    colClasses = "character",
                    stringsAsFactors = FALSE)
  ## get unique set of locations and targets as df
  set <- entry %>%
    dplyr::group_by(location,target) %>%
    dplyr::select(location,target) %>%
    dplyr::ungroup() %>%
    unique()
  
  ## get model name and forecast week from filename
  model_name <- substr(basename(sanitized_file),11,nchar(basename(sanitized_file))-4)
  forecast_week <- as.numeric(gsub("ew", "", 
                                 regmatches(sanitized_file, regexpr("(?:ew)[0-9]{2}", sanitized_file))))
  date_file <- cdcForecastUtils::covid_19_forecast_dates
  set$model_name <- model_name
  set$forecasts_due <- date_file$forecasts_due[
    which(as.numeric(gsub("EW", "", regmatches(date_file$ilinet_data_thru_ew, 
                                               regexpr("(?:EW)[0-9]{2}", date_file$ilinet_data_thru_ew))))==forecast_week)]
  return(set)
}