#' Compute first week when incidence falls below baseline and remains below for
#' at least three weeks in a row.
#'
#' @param trajectory a numeric vector with incidence for each time point in a 
#'   season
#' @param baseline the threshold that incidence must cross
#' @param lookahead_length number of consecutive time points that incidence must
#'   fall below the baseline threshold (default 3)
#'
#' @return the smallest index i such that every entry of
#'   trajectory[seq(from = i, length = lookahead_length)] is < baseline,
#'   if such an index exists.  Otherwise, NA_integer_
#'
#' @export
get_below_baseline_idx <- function (
  trajectory,
  baseline,
  lookahead_length
) {
  below_baseline_by_ind <- sapply(
    seq_len(length(trajectory) - lookahead_length),
    function(start_ind) {
      below_baseline <-
        trajectory[seq(from = start_ind, length = lookahead_length)] < baseline
      
      return(
        (length(below_baseline) > 0) &&
        all(below_baseline) &&
        !all(is.na(trajectory))
      )
    }
  )
  
  if(any(below_baseline_by_ind, na.rm = TRUE)) {
    idx <- min(which(below_baseline_by_ind))
    
    return(idx)
  } else {
    return(NA_integer_)
  }
}
