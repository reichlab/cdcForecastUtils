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
    dplyr::filter(type == "bin",target!="Below baseline for 3 weeks") %>%
    dplyr::mutate(value=as.numeric(as.character(value))) %>%
    dplyr::group_by(location, target) %>%
    dplyr::summarize(
              miss     = any(is.na(value)),
              sum      = sum(value, na.rm = TRUE),
              negative = any(!is.na(value) & value < 0))
  
  probabilities_binary <- entry %>%
    dplyr::filter(type == "bin",target=="Below baseline for 3 weeks") %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(
      miss     = any(is.na(value)),
      more_than_one = any(!is.na(value) & value > 1),
      negative = any(!is.na(value) & value < 0))
  
  errors <- character()
  # Report message for missing probabilities
  # fix for intentional missing for targets not forecasted
  if (any(probabilities$miss)) {
    tmp <- probabilities %>%
      dplyr::filter(miss)
    errors <- c(errors, paste0("ERROR: Missing probabilities detected in ",
                                 paste(tmp$location, tmp$target), ".\n"))
  }
  if (any(probabilities_binary$miss)) {
    tmp <- probabilities_binary %>%
      dplyr::filter(miss)
    errors <- c(errors, paste0("ERROR: Missing probabilities detected in ",
                               paste(tmp$location, tmp$target), ".\n"))
  }
  # Report message for negative probabilities
  if (any(probabilities$negative)) {
    tmp <- probabilities %>%
      dplyr::filter(negative)
    
    errors <- c(errors, paste0("ERROR: Negative probabilities detected in ",
                               paste(tmp$location, tmp$target), ".\n"))
  }
  if (any(probabilities_binary$negative)) {
    tmp <- probabilities_binary %>%
      dplyr::filter(negative)
    
    errors <- c(errors, paste0("ERROR: Negative probabilities detected in ",
                               paste(tmp$location, tmp$target), ".\n"))
  }
  # Report message for sum of target probabilities outside of 0.9 and 1.1
  if (any(probabilities$sum < 0.9 | probabilities$sum > 1.1)) {
    tmp <- probabilities %>%
      dplyr::filter(sum<0.9 | sum>1.1)
    
    errors <- c(errors, paste0("ERROR: In ", tmp$location, "-", tmp$target, ", probabilities sum to ",
                               tmp$sum, ". \n"))
  }
  if (any(probabilities_binary$more_than_one)) {
    tmp <- probabilities_binary %>%
      dplyr::filter(more_than_one)
    
    errors <- c(errors, paste0("ERROR: Higher than 1 binary probabilities detected in ",
                               paste(tmp$location, tmp$target), ".\n"))
  }
  #Output probability related errors
  if (length(errors) != 0) {
    stop(errors)
  }

  return(invisible(TRUE))
}