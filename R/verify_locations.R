#' Verify the locations
#'
#' Compares locations to full entries and provides an error if all valid locations
#' are missing, a warning if there are any extra locations, and a message to
#' indicate locations with no forecast.
#'
#' @param entry An entry data.frame
#' @return Invisibly returns \code{TRUE} if successful
#' @param challenge one of "ilinet" or "state_ili" or "hospitalization", indicating which
#'   challenge the submission is for
#' @export
#' @keywords internal
verify_locations <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "state_ili", "hospitalization"))) {
    stop("challenge must be one of ilinet or state_ili or hospitalization")
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
  
  if (challenge == "hospitalization"){
    state_country_code <- expand.grid(str_pad(unique(cdcForecastUtils::hospitalization_locations$state_code),width =2,pad = "0"),
                                      str_pad(unique(cdcForecastUtils::hospitalization_locations$county_code),width =3,pad = "0"))
    valid_locations <- c(paste0(state_country_code$Var1 ,state_country_code$Var2),
                         str_pad(unique(cdcForecastUtils::hospitalization_locations$state_code),width =2,pad = "0"),
                         "US")
  }
  
  # Identify missing locations and throw error
  # missing_locations <- setdiff(required_locations, entry_locations)
  # if (length(missing_locations) > 0)
  #   stop("Missing these locations: ", paste(missing_locations))
  
  # Determine extra locations and non-required missing locations
  extra_locations   <- setdiff(entry_locations, valid_locations)
  possible_locations <- setdiff(valid_locations, entry_locations)
  has_error <- FALSE
  
  if (length(intersect(entry_locations, valid_locations))==0){
    warning("Missing all valid locations. Wrong challenge?: ", paste(extra_locations,collapse=", "))
    has_error <- TRUE
  }
  if (length(extra_locations)>0){ 
    warning("These extra locations are ignored. Please check for possible spelling errors: ", 
            paste(extra_locations,collapse=", "))
    has_error <- TRUE
  }
  if (length(possible_locations)>0 & challenge != "hospitalization")
    # message("Consider forecasting for these locations: ", paste(possible_locations))
    message("Please check if this is intended - these locations have no forecast: ", 
            paste(possible_locations, collapse = ", "))
  
  if (has_error) {
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE))
  }
}