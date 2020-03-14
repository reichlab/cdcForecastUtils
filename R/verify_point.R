#' Verify validity of point predictions
#'
#' @param entry An entry data.frame
#' @import dplyr
#' @return Invisibly returns \code{TRUE} or a descriptive warning/error 
#' message
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples 
#' verify_point(full_entry_new)
verify_point <- function(entry) {
  
  names(entry) <- tolower(names(entry))
  
  point <- entry %>%
    dplyr::filter(type == "point") %>%
    dplyr::mutate(miss     = is.na(value),
           negative = (!is.na(value) & value < 0))
  
  # Report warning for missing point predictions
  if (any(point$miss)) {
    tmp <- point %>%
      dplyr::filter(miss)
    
    warning(paste0("WARNING: Missing point predictions detected in ",
                   paste(tmp$location, tmp$target), ". \n",
                   "Please take a look at FluSight::generate_point_forecasts().\n"))
  }
  
  # Report error for negative point predictions
  if (any(point$negative)) {
    tmp <- point %>%
      dplyr::filter(negative)
    
    stop(paste0("ERROR: Negative point predictions detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at FluSight::generate_point_forecasts().\n"))
  }
  
  return(invisible(TRUE))
}