#' Read in a csv entry file
#'
#' This function reads in the csv file and arranges it for consistency.
#'
#' @param file A csv file path
#' @return An arranged data.frame
#' @import dplyr
#' @export
read_entry = function(file) {
  message("The file has been re-formatted.")
  entry <- read.csv(file, 
                    colClasses = "character",
                    stringsAsFactors = FALSE)
  # # Add forecast week to imported data - Not added at this moment
  # forecast_week <- as.numeric(gsub("ew", "", 
  #                                  regmatches(file, regexpr("(?:ew)[0-9]{2}", file))))
  # if (length(forecast_week > 0))
  #   entry <- dplyr::mutate(entry, forecast_week  = forecast_week)
  # 
  entry1 <- cdcForecastUtils::sanitize_entry(entry)
  entry2 <- cdcForecastUtils::arrange_entry(entry1)
  return(entry2)
}

#' Arrange an entry for consistency
#'
#' @param entry A data.frame
#' @return An arranged data.frame
#' @import dplyr
#' @export
#' @keywords internal
arrange_entry <- function(entry) {
  # Arrange entry by type, location, target, bin
  entry %>%
    dplyr::arrange(type, location, target) %>%
    dplyr::select(location, target, type, bin, value, everything())
}