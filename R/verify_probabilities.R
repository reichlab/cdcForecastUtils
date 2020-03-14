#' Verify the entry probabilities
#'
#' @param entry An entry data.frame
#' @import dplyr
#' @return Invisibly returns \code{TRUE} or a descriptive error message
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples 
#' verify_probabilities(full_entry_new)
#' 
verify_probabilities <- function(entry) {
  
  names(entry) <- tolower(names(entry))
  
  probabilities <- entry %>%
    filter(type == "bin") %>%
    group_by(location, target) %>%
    summarize(
              miss     = any(is.na(value)),
              missnum  = sum(is.na(value)),
              sum      = sum(value, na.rm = TRUE),
              negative = any(!is.na(value) & value < 0))
  
  errors <- character()
  check <- character()
  # Report message for missing probabilities
  # fix for intentional missing for targets not forecasted
  if (any(probabilities$miss)) {
    tmp <- probabilities %>%
      filter(miss)
    # the length should be 251
    if (probabilities$missnum == 251) {
      check <- c(check, paste0("Please check if this is intended - No forecast probabilities for ",
                                paste(tmp$location, tmp$target), ".\n"))
      
    } else {
      errors <- c(errors, paste0("ERROR: Sporadic missing probabilities detected in ",
                                 paste(tmp$location, tmp$target), ".\n"))
    }
  }
  
  # Report message for negative probabilities
  if (any(probabilities$negative)) {
    tmp <- probabilities %>%
      filter(negative)
    
    errors <- c(errors, paste0("ERROR: Negative probabilities detected in ",
                               paste(tmp$location, tmp$target), ".\n"))
  }
  
  # Report message for sum of target probabilities outside of 0.9 and 1.1
  if (any(probabilities$sum < 0.9 | probabilities$sum > 1.1)) {
    tmp <- probabilities %>%
      filter(sum<0.9 | sum>1.1)
    
    errors <- c(errors, paste0("ERROR: In ", tmp$location, "-", tmp$target, ", probabilities sum to ",
                               tmp$sum, ". \n"))
  }
  
  #Output probability related errors
  if (length(errors) != 0) {
    stop(errors)
  }
  if (length(check) != 0) {
    message(check)
  }
  return(invisible(TRUE))
}