#' Verify an entry file
#'
#' This function will check to make sure the structure is correct and that
#' the forecast probabilities are non-negative and sum to a value between
#' 0.9 and 1.1. For binary targets, this function will check if the forecast
#' provavilities are between 0 and 1.
#'
#' @param file A csv entry file
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @seealso \code{\link{verify_entry}}
#' @examples
#' file <- system.file("extdata/EW10-2019-valid_national_template.csv",package="cdcForecastUtils")
#' verify_entry_file(file) # TRUE
verify_entry_file <- function(file, challenge = "ilinet") {
  entry <- cdcForecastUtils::read_entry(file)
  model_name<-basename(file)
  if(cdcForecastUtils::verify_entry(entry, challenge)){
    return(message(paste0(model_name," passed verification tests")))
  } else {
    return(message(paste0(model_name," failed verification tests")))
  }
}

#' Verify entry stored as an R data.frame
#'
#' @param entry A data.frame
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for
#' @param check_week A logical value (default `TRUE`) indicating whether to check
#'   for the column forecast_week. Should be `TRUE` if evaluating entry prior to 
#'   scoring, can be `FALSE` if evaluating entry prior to writing to disk.
#' @return Invisibly returns \code{TRUE} if successful
#' @import dplyr
#' @export
#' @seealso \code{\link{verify_entry_file}}
#' @examples
#' verify_entry(full_entry_new)
verify_entry <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "state_ili"))) {
    stop("challenge must be one of ilinet or state_ili")
  }
  
  names(entry) <- tolower(names(entry))
  
  cdcForecastUtils::verify_colnames(entry)
  
  # Verify column contents
  cdcForecastUtils::verify_locations(entry, challenge)
  cdcForecastUtils::verify_targets(entry, challenge)
  cdcForecastUtils::verify_types(entry, challenge)
  cdcForecastUtils::verify_bins(entry, challenge)
  cdcForecastUtils::verify_probabilities(entry)
  cdcForecastUtils::verify_point(entry)
  
  return(invisible(TRUE))
}