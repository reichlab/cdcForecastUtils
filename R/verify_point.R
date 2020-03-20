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
    # add target to allow NA for first week below baseline if none is in allowed week range
    dplyr::filter(type == "point",target %in% c("1 wk ahead","2 wk ahead","3 wk ahead","4 wk ahead",
                                                "5 wk ahead","6 wk ahead","Peak height")) %>%
    dplyr::mutate(miss = is.na(value),
           negative = (!is.na(value) & suppressWarnings(as.numeric(value)) < 0))
  # check missing for peak week
  point_peakweek <- entry %>%
    # add target to allow NA for first week below baseline if none is in allowed week range
    dplyr::filter(type == "point",target == "Peak week") %>%
    dplyr::mutate(miss = is.na(value))
  point_char <- entry %>%
    dplyr::filter(type == "point",target %in% c("Peak week","First week below baseline")) %>%
    dplyr::mutate(weekrange=ifelse(!(is.na(value)), 
                               as.numeric(substr(value,8,10))),
                  check_range=(weekrange>35 |weekrange<10),
                  check_char=(regmatches(value,regexpr("2020-ew",value))!="2020-ew")
                  )
  
  # Report warning for missing point predictions
  if (any(point$miss)) {
    tmp <- point %>%
      dplyr::filter(miss)
    
    warning(paste0("WARNING: Missing point predictions detected in ",
                   paste(tmp$location, tmp$target), ". \n",
                   "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
  }
  if (any(point_peakweek$miss)) {
    tmp <- point_peakweek %>%
      dplyr::filter(miss)
    
    stop(paste0("ERROR: Missing point predictions detected in ",
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
  
  if (any(point_char$check_char)) {
    tmp <- point_char %>%
      dplyr::filter(point_char)
    
    stop(paste0("ERROR: Incorrect point prediction format for week targets detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at the template and at cdcForecastUtils::generate_point_forecasts().\n"))
  }
  return(invisible(TRUE))
}