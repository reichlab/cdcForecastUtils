#' Verify targets are correct
#'
#' The targets depend on the challenge, so this will verify targets are correct
#' for all targets based on the challenge the submission is for
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for
#' @return Invisibly returns TRUE if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_targets(full_entry_new)
verify_targets <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "state_ili","hospitalization"))) {
    stop("challenge must be one of ilinet or state_ili or hospitalization")
  }
  
  names(entry) <- tolower(names(entry))
  
  # ILINet challenge
  if (challenge == "ilinet") {
    valid_targets <- unique(cdcForecastUtils::full_entry_new$target)
  } else if (challenge == "state_ili") {
    valid_targets <- unique(cdcForecastUtils::full_entry_state_new$target)
  } else if (challenge == "hospitalization"){
    valid_targets <- unique(cdcForecastUtils::hosp_template$target)
  }
  
  entry_targets <- unique(entry$target)
  
  missing_targets <- setdiff(valid_targets, entry_targets)
  extra_targets   <- setdiff(entry_targets, valid_targets)
  has_error <- FALSE
  if (length(missing_targets)>0)
    # message not warning or stop because it might be intentional
    message("Please check if this is intended - Missing these targets: ", paste(missing_targets, collapse=", "))
  
  if (length(extra_targets)>0) {
    warning("These extra targets are not valid. Please check capitalization or spelling: ", paste(extra_targets, collapse=", "))
    has_error <- TRUE
  }
  
  if (has_error) {
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE))
  }
}