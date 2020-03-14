#' Verify the locations
#'
#' Compares locations to minimal and full entries and provides an error if any
#' locations are missing, a warning if there are any extra locations, and a
#' message to suggest forecasting for additional locations.
#'
#' @param entry An entry data.frame
#' @return Invisibly returns \code{TRUE} if successful
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for
#' @export
#' @keywords internal
verify_locations <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "state_ili"))) {
    stop("challenge must be one of ilinet or state_ili")
  }
  
  names(entry) <- tolower(names(entry))
  entry_locations <- unique(entry$location)
  
  if (challenge == "ilinet") {
    # required_locations <- unique(FluSight::minimal_entry$location)
    # assume no required location
    valid_locations    <- unique(cdcForecastUtils::full_entry_new$location)
  }
  
  if (challenge == "state_ili") {
    # required_locations <- NULL
    valid_locations <- unique(cdcForecastUtils::full_entry_state_new$location)
  }
  
  # Identify missing locations and throw error
  # missing_locations <- setdiff(required_locations, entry_locations)
  # if (length(missing_locations) > 0)
  #   stop("Missing these locations: ", paste(missing_locations))
  
  # Determine extra locations and non-required missing locations
  extra_locations   <- setdiff(entry_locations, valid_locations)
  possible_locations <- setdiff(valid_locations, entry_locations)
  if (length(intersect(entry_locations, valid_locations))==0){
    stop("Missing all valid locations. Wrong challenge?: ", paste(extra_locations))
  }
  if (length(extra_locations)>0){ 
    warning("These extra locations are ignored. Please check for possible spelling errors: ", paste(extra_locations))
  }
  if (length(possible_locations)>0)
    # message("Consider forecasting for these locations: ", paste(possible_locations))
    message("These locations have no forecast: ", paste(possible_locations))
  
  return(invisible(TRUE))
}