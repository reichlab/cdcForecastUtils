#' Verify bins are correct
#'
#' The necessary bins depend on the target, so this will verify bins are correct
#' for all targets
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet" or "state_ili" or "hospitalization", indicating which
#'   challenge the submission is for
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_bins(full_entry_new)
verify_bins <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "state_ili","hospitalization"))) {
    stop("challenge must be one of ilinet or state_ili or hospitalization")
  }
  
  if (challenge == "ilinet") {
    valid <- cdcForecastUtils::full_entry_new
  } else if (challenge == "state_ili") {
    valid <- cdcForecastUtils::full_entry_state_new
  } else if (challenge == "hospitalization"){
    valid <- cdcForecastUtils::hosp_template
  }
  
  entry_targets <- unique(entry$target)  
  
  errors <- character()
  errors_x <- character()
  
  for(i in seq_along(entry_targets)) {
    entry_bins <- unique(entry$bin[entry$target == entry_targets[i]])
    
    valid_bins <- unique(valid$bin[valid$target == entry_targets[i]])

    missing_bins <- setdiff(valid_bins, entry_bins)
    extra_bins <- setdiff(entry_bins, valid_bins)
    
    if (length(missing_bins) > 0)
      errors <- c(errors, paste0("Check bin range or format. If this is NA, there is a point prediction - missing valid bins for ", 
                                 entry_targets[i], ": ", missing_bins, "\n"))
    
    if (length(extra_bins) > 0)
      errors_x <- c(errors_x, paste0("Check bin range or format - these bins for ",
                                     entry_targets[i], " are invalid: ",
                                     extra_bins, "\n"))
  }
  
  if (length(errors) > 0)
    stop(errors)
  
  if (length(errors_x) > 0)
    stop(errors_x)
  
  return(invisible(TRUE))
}