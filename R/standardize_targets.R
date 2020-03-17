#' Convert targets to standardize form
#' 
#' @param targets: character vector of targets
#' 
#' @return character vector of valid targets
standardize_targets <- function(targets) {
  valid_targets <- c("wk ahead", "Below baseline for 3 weeks",
                     "First week below baseline", "Peak height", "Peak week")
  valid_targets_lower <- tolower(valid_targets)
  
  user_targets <- match.arg(tolower(targets), valid_targets_lower, several.ok = TRUE)
  
  if(length(user_targets) < length(targets)) {
    warning(paste0("Unsupported target requested; targets must be one or more of: ",
                   paste(valid_targets, collapse = ", ")))
  }
  
  targets <- valid_targets[valid_targets_lower %in% user_targets]
  
  return(targets)
}