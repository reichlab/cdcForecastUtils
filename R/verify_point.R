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
                               as.numeric(substr(value,8,10))),
                  check_range=(weekrange>35 |weekrange<10),
                  check_char=(regmatches(value,regexpr("2020-EW",value))!="2020-EW")
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
  if (any(point[point$target %in% c(paste(1:4, " wk ahead")),]$negative)) {
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
  
  if (any(point_char$check_char)) {
    tmp <- point_char %>%
      dplyr::filter(point_char)
    
    stop(paste0("ERROR: Incorrect point prediction format for week targets detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at the template and at cdcForecastUtils::generate_point_forecasts().\n"))
  }
  return(invisible(TRUE))
}