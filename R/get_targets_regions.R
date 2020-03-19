#' Save submitted targets/locations of a file
#'
#' This function will check to make sure the structure is correct and that
#' the forecast probabilities are non-negative and sum to a value between
#' 0.9 and 1.1. For binary targets, this function will check if the forecast
#' provavilities are between 0 and 1.
#'
#' @param file A csv entry file path
#' @return A data.frame of a set of submitted targets/locations of a model
#' @import dplyr
#' @export
get_targets_regions<- function(sanitized_file) {
  entry <- read.csv(sanitized_file, 
                    colClasses = "character",
                    stringsAsFactors = FALSE)
  set <- entry %>%
    dplyr::group_by(location,target) %>%
    dplyr::select(location,target) %>%
    dplyr::ungroup() %>%
    unique()
  model_name<-substr(basename(sanitized_file),1,nchar(basename(sanitized_file))-4)
  forecast_week<-as.numeric(gsub("EW", "", 
                                 regmatches(sanitized_file, regexpr("(?:EW)[0-9]{2}", sanitized_file))))
  date_file <- cdcForecastUtils::covid_19_forecast_dates
  set$model_name <- model_name
  set$forecasts_due <- date_file$forecasts_due[
    which(as.numeric(gsub("EW", "", regmatches(date_file$ilinet_data_thru_ew, 
                                               regexpr("(?:EW)[0-9]{2}", date_file$ilinet_data_thru_ew))))==forecast_week)]
  return(set)
}