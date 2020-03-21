#' Save submitted targets/locations of a file
#'
#' This function will retrieve information from submitted model 
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
  model_name<-substr(basename(sanitized_file),11,nchar(basename(sanitized_file))-4)
  formatted_filename <- ifelse(grepl("2020-EW[0-9]{2}",sanitized_file),
                               gsub("2020-EW",replacement="2020-ew",sanitized_file),sanitized_file)
  forecast_week<-as.numeric(gsub("ew", "", 
                                 regmatches(formatted_filename, regexpr("(?:ew)[0-9]{2}", formatted_filename))))
  date_file <- cdcForecastUtils::covid_19_forecast_dates
  set$model_name <- model_name
  set$forecasts_due <- date_file$forecasts_due[
    which(as.numeric(gsub("EW", "", regmatches(date_file$ilinet_data_thru_ew, 
                                               regexpr("(?:EW)[0-9]{2}", date_file$ilinet_data_thru_ew))))==forecast_week)]
  return(set)
}