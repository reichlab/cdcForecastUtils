#' Verify validity of point predictions
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for
#' @import dplyr
#' @return Invisibly returns \code{TRUE} or a descriptive warning/error 
#' message
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples 
#' verify_point(full_entry_new)
verify_point <- function(entry,challenge="ilinet") {
  
  names(entry) <- tolower(names(entry))
  if (challenge == "ilinet" | challenge == "state_ili"){
    point_targets <-  c("1 wk ahead","2 wk ahead","3 wk ahead","4 wk ahead",
                        "5 wk ahead","6 wk ahead","Peak height")
    week_targets <- c("Peak week","First week below baseline")
  } else{
    point_targets <-  c(paste0(1:25," wk ahead"),"Peak hospitalizations")
    week_targets <- c("Peak week")
  }
  
  point <- entry %>%
    # add target to allow NA for first week below baseline if none is in allowed week range
    dplyr::filter(type == "point",target %in% point_targets) %>%
    dplyr::mutate(miss = is.na(value),
           negative = (!is.na(value) & suppressWarnings(as.numeric(value)) < 0))
  # check missing for peak week
  point_char <- entry %>%
    dplyr::filter(type == "point",target %in% week_targets) %>%
    dplyr::mutate(miss = is.na(value),
      weekformat=ifelse(!(is.na(value)), !(grepl("2020-ew[0-9]{2}", value)), FALSE)) %>%
    dplyr::mutate(weekrange=ifelse((!(is.na(value)) &  grepl("2020-ew[0-9]{2}", value)),
                               as.numeric(substr(value,8,10)),NA),
                  check_range=ifelse((!(is.na(weekrange)) ),(weekrange>35 |weekrange<10),FALSE))
  has_error <- FALSE
  
  # Report warning for missing point predictions
  if (any(point$miss)) {
    tmp <- point %>%
      dplyr::filter(miss)
    
    warning(paste0("WARNING: Missing point predictions detected in ",
                   paste(tmp$location, tmp$target), ". \n",
                   "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
    has_error <- TRUE
  }
  if (any(point_char$miss)) {
    tmp <- point_char %>%
      dplyr::filter(miss)
    
    warning(paste0("ERROR: Missing point predictions detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
    has_error <- TRUE
  }
  # Report error for negative point predictions
  if (any(point$negative)) {
    tmp <- point %>%
      dplyr::filter(negative)
    
    warning(paste0("ERROR: Negative point predictions detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
    has_error <- TRUE
  }

  # Report error for out of range week
  if (any(point_char$check_range)) {
    tmp <- point_char %>%
      dplyr::filter(check_range)
    
    warning(paste0("ERROR: Out-of-range point predictions for week targets detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
    has_error <- TRUE
  }
  # Report week format error
  if (any(point_char$weekformat)) {
    tmp <- point_char %>%
      dplyr::filter(weekformat)
    
    warning(paste0("ERROR: Empty, incorrect format or season for week target's point predictions detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at cdcForecastUtils::generate_point_forecasts().\n"))
    has_error <- TRUE
  }
  
  if (has_error) {
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE))
  }
}