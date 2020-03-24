#' Verify bins are correct
#'
#' The necessary bins depend on the target, so this will verify bins are correct
#' for all targets
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_bins(full_entry_new)
verify_bins <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "state_ili"))) {
    stop("challenge must be one of ilinet or state_ili")
  }
  
  if (challenge == "ilinet") {
    valid <- cdcForecastUtils::full_entry_new
  } else {
    valid <- cdcForecastUtils::full_entry_state_new
  }
  
  entry_targets <- unique(entry$target)  
  
  errors <- character()
  errors_x <- character()
  
  for(i in seq_along(entry_targets)) {
    entry_bins <- unique(entry$bin[entry$target == entry_targets[i] & 
                                                entry$type == "bin"])
    
    valid_bins <- unique(valid$bin[valid$target == entry_targets[i] &
                                                valid$type == "bin"])
    missing_bins <- setdiff(valid_bins, entry_bins)
    extra_bins <- setdiff(entry_bins, valid_bins)
    
    if (length(missing_bins) > 0)
      errors <- c(errors, paste0("Bin column must be NA for point predictions. Check bin range or format- missing valid bins for ", 
                                 entry_targets[i], ": ", missing_bins, "\n"))
    
    if (length(extra_bins) > 0)
      errors_x <- c(errors_x, paste0("Bin column must be NA for point predictions. Check bin range or format - these bins for ",
                                     entry_targets[i], " are invalid: ",
                                     extra_bins, "\n"))
  }
  
  if (length(errors) > 0)
    stop(errors)
  
  if (length(errors_x) > 0)
    stop(errors_x)
  
  return(invisible(TRUE))
}