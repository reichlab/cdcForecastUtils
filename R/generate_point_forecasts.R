#' Generate point forecasts for all locations and targets
#'
#' @param entry An entry data.frame
#' @param method The method to be used to generate the point forecasts. 
#'   \code{"Median"} (the default) uses the median value, \code{"Expected
#'   Value"} generates the expected value from the provided probabilities,and
#'   \code{"Mode"} returns the individual bin with the largest probability
#' @return A data.frame of point forecasts for all locations and targets.
#' @seealso \code{\link{generate_point_forecast}}, \code{\link{verify_entry}}
#' @import magrittr
#' @export
generate_point_forecasts <- function(entry, method = 
                                       c("Median", "Expected Value", "Mode")) {
  method <- match.arg(method)
  names(entry) <- tolower(names(entry))
  if (sum(entry$type == "point") > 0) {
    warning("It appears point forecasts already exist.")
  }
  # Generate point forecasts
  entry <- entry %>%
    dplyr::filter(type == "bin") %>%
    dplyr::group_by(location, target) %>%
    cdcForecastUtils::generate_point_forecast(., method) %>%
    dplyr::ungroup()
  return(entry)
  
}


#' Generate a point forecast from probabilistic forecast
#'
#' The point forecast is taken to be either the expected value, median, 
#' or mode of the probabilistic forecasts.
#'
#' @param d A data.frame with columns `location`, `target`, and `value`
#' @param method The method to be used to generate the point forecasts. 
#'   \code{"Median"} (the default) uses the median value, \code{"Expected
#'   Value"} generates the expected value from the provided probabilities,and
#'   \code{"Mode"} returns the individual bin with the largest probability 
#' @return A data.frame with columns `type` and `value`
#' @seealso \code{\link{generate_point_forecasts}}
#' @import magrittr
#' @export
#' @keywords internal
generate_point_forecast <- function(d, method = 
                                      c("Median", "Expected Value", "Mode")) {
  
  if (packageVersion("dplyr") < "0.7.0") {
    stop("dplyr >= 0.7.0 needed for this function.", call. = FALSE)
  }
  
  method <- match.arg(method)
  
  names(d) <- tolower(names(d))
  
  # comment out since this is not a regular season
  # Find max MMWR week in submitted entry
  # maxMMWR <- d %>%
  #   dplyr::filter(target %in% c("Peak week","First week below baseline")) %>%
  #   stats::na.omit() %>%
  #   dplyr::mutate(bin = as.numeric(gsub("EW", "", regmatches(bin, regexpr("(?:EW)[0-9]{2}", bin))))) %>%
  #   dplyr::pull(bin) %>%
  #   max()
  
  # get a data frame for non-week targets
  d1 <- d %>% 
    dplyr::filter(!(target %in% c("Peak week","First week below baseline","Below baseline for 3 weeks"))) %>%
    dplyr::mutate(bin = suppressWarnings(as.numeric(bin)),value = suppressWarnings(as.numeric(value)))
  
  # get a data frame for week targets, assuming we don't go into the fall
  d2 <- d %>% 
    dplyr::filter(target %in% c("Peak week","First week below baseline")) %>%
    dplyr::mutate(bin=replace(bin, !(is.na(bin)), 
                              gsub("EW", "", regmatches(bin, regexpr("(?:EW)[0-9]{2}", bin))))) %>%
    dplyr::mutate(bin=as.numeric(bin),value = suppressWarnings(as.numeric(value)))
  
  # combine with bin being numeric
  d3 <- rbind(d1,d2) %>%
    dplyr::arrange(location, target, bin)
  
  # Expected Value method
  if (method == "Expected Value") {
    temp <- d3 %>%
      stats::na.omit() %>% # Remove the NA for no onset to calculate mean
      dplyr::mutate(probability = value/sum(value),
                    value       = as.numeric(bin)) %>%
      dplyr::summarize(value = sum(value*probability)) %>%
      dplyr::mutate(type = "point",
                    value = ifelse(target %in% c("Peak week","First week below baseline"),
                                   paste0("2020-EW",round(value, 0)),
                                   value))
  }
  
  # Median method
  if (method == "Median") {
    temp <- d %>%
      dplyr::mutate(cumulative = cumsum(value),
                    type = "point") %>%
      dplyr::filter(dplyr::row_number() == min(which(cumulative >= 0.5))) %>%
      dplyr::select(location, target, value = bin, type) %>%
      dplyr::mutate(value = ifelse(target %in% c("Peak week","First week below baseline"),
                                   paste0("2020-EW",value),value))
  }
  
  # Mode method
  if (method == "Mode") {
    temp <- d %>%
      dplyr::filter(value == max(value)) %>%
      dplyr::select(location, target, value = bin, type) %>%
      dplyr::mutate(type = "point",
                    value = ifelse(target %in% c("Peak week","First week below baseline"),
                                   paste0("2020-EW",value),value))
  }
  
  return(temp)
}