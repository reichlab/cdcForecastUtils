#' Verify targets are correct
#'
#' The necessary targets depend on the challenge, so this will verify targets are correct
#' for all targets
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
  
  if (!(challenge %in% c("ilinet", "state_ili"))) {
    stop("challenge must be one of ilinet or state_ili")
  }
  
  names(entry) <- tolower(names(entry))
  
  # ILINet challenge
  if (challenge == "ilinet") {
    valid_targets <- unique(cdcForecastUtils::full_entry_new$target)
  } else if (challenge == "state_ili") {
    valid_targets <- unique(cdcForecastUtils::full_entry_state_new$target)
  } 
  
  entry_targets <- unique(entry$target)
  
  missing_targets <- setdiff(valid_targets, entry_targets)
  extra_targets   <- setdiff(entry_targets, valid_targets)
  
  if (length(missing_targets)>0)
    # message not warning or stop because it might be intentional
    message("Please check if this is intended - Missing these targets: ", paste(missing_targets))
  
  if (length(extra_targets)>0)
    warning("These extra targets are not valid and are ignored: ", paste(extra_targets))
  
  return(invisible(TRUE))
}