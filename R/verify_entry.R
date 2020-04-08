#' Verify an entry file
#'
#' This function will check to make sure the structure is correct and that
#' the forecast probabilities are non-negative and sum to a value between
#' 0.9 and 1.1. For binary targets, this function will check if the forecast
#' provavilities are between 0 and 1.
#'
#' @param file A csv entry file
#' @param challenge one of "ilinet" or "state_ili" or "hospitalization", indicating which
#'   challenge the submission is for
#' @return Invisibly returns \code{TRUE} if successful, \code{FALSE} if not
#' @export
#' @seealso \code{\link{verify_entry}}
#' @examples
#'\dontrun{
#' file <- system.file("extdata/2020-ew10-valid-national.csv",package="cdcForecastUtils")
#' verify_entry_file(file) 
#' }
verify_entry_file <- function(file, challenge = "ilinet") {

  filename <- basename(file)
  
  ## verify filename conventions
  cdcForecastUtils::verify_filename(filename, challenge)
  
  ## read and verify entry contents
  entry <- cdcForecastUtils::read_entry(file)
  if(cdcForecastUtils::verify_entry(entry, challenge)){
    message(paste0(filename," passed verification tests"))
    return(invisible(TRUE))
  } else {
    warning(paste0(filename," failed verification tests"))
    return(invisible(FALSE))
  }
}

#' Verify entry stored as an R data.frame
#'
#' @param entry A data.frame
#' @param challenge one of "ilinet" or "state_ili" or "hospitalization", indicating which
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
  
  if (!(challenge %in% c("ilinet", "state_ili","hospitalization"))) {
    stop("challenge must be one of ilinet or state_ili or hospitalization ")
  }
  
  names(entry) <- tolower(names(entry))
  
  cdcForecastUtils::verify_colnames(entry)
  
  # Verify column contents

  verified_locations <- cdcForecastUtils::verify_locations(entry, challenge)
  verified_targets <- cdcForecastUtils::verify_targets(entry, challenge)
  verified_types <- cdcForecastUtils::verify_types(entry, challenge)
  verified_bins <- cdcForecastUtils::verify_bins(entry, challenge)
  verified_probabilities <- cdcForecastUtils::verify_probabilities(entry)
  verified_point <- cdcForecastUtils::verify_point(entry)

  if (verified_locations && verified_targets && verified_types && verified_bins && verified_probabilities && verified_point)
    return(invisible(TRUE))
  stop("Entry did not pass all verification test")
}