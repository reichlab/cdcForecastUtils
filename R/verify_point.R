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
    dplyr::mutate(miss = is.na(value),
           negative = (!is.na(value) & suppressWarnings(as.numeric(value)) < 0))
  
  point_char <- entry %>%
    dplyr::filter(type == "point",target %in% c("Peak week","First week below baseline")) %>%
    dplyr::mutate(weekrange=ifelse(!(is.na(value)), 
                               gsub("EW", "", regmatches(bin, regexpr("(?:EW)[0-9]{2}", bin))),value),
                  check_range=(weekrange>35 |weekrange<10)
                  )
  
  # Report warning for missing point predictions
  if (any(point$miss)) {
    tmp <- point %>%
      dplyr::filter(miss)
    
    warning(paste0("WARNING: Missing point predictions detected in ",
                   paste(tmp$location, tmp$target), ". \n",
                   "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
  }
  
  # Report error for negative point predictions
  if (any(point$negative)) {
    tmp <- point %>%
      dplyr::filter(negative)
    
    stop(paste0("ERROR: Negative point predictions detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
  }
  # Report error for out of range week
  if (any(point_char$check_range)) {
    tmp <- point_char %>%
      dplyr::filter(point_char)
    
    stop(paste0("ERROR: Out-of-range point predictions for week targets detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
  }
  return(invisible(TRUE))
}