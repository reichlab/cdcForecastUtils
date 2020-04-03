#' Create a data frame containing a row with a log score column for use during a season
#'
#' @param file A file path
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for
#' @param targets a set of week ahead targets to score
#' @param start_ew a starting week of a challenge
#' @return data.frame with log scores
#' @import dplyr 
#' @export
internal_logscore <- function(file, challenge, targets, start_ew) {
  message("this currently works for some week ahead targets only")
  if (!(challenge %in% c("ilinet", "state_ili"))) {
    stop("challenge must be one of ilinet, or state_ili")
  }
  if(challenge=="ilinet"){full_truth <- download_and_preprocess_flu_data()} else
  {full_truth <-download_and_preprocess_state_flu_data()}
  forecast_week <- as.numeric(gsub("ew", "", regmatches(file, regexpr("(?:ew)[0-9]{2}", file))))
  entry <- read_entry(file) %>%
    dplyr::filter(target %in% targets,type=="bin")
  week_range <- c(start_ew:full_truth$week[nrow(full_truth)])
  if (challenge=="ilinet") {
    truth <- full_truth %>%
      dplyr::mutate(location=ifelse(region=="National",paste0("US ",region),paste0("HHS ",region))) %>%
      dplyr::filter(year==substr(basename(file), start = 1, stop = 4), week %in% week_range, 
                    location %in% unique(entry$location)) %>%
      dplyr::select(location,week,weighted_ili)
  } else {
    truth <- full_truth %>%
      dplyr::filter(year==substr(basename(file), start = 1, stop = 4), week %in% week_range, 
                    region %in% unique(entry$location)) %>%
      dplyr::select(region,week,unweighted_ili)
  }

  # Add forecast week to imported data
  if (length(forecast_week > 0)) entry <- dplyr::mutate(entry, forecast_week  = forecast_week)
  if (challenge=="ilinet") {
    scored_entry <- entry %>%
      dplyr::mutate(week_match=as.numeric(substr(target,start=1,stop=1))+forecast_week,
                    bin_end=ifelse(as.numeric(bin)!=25,as.numeric(bin)+0.1,100)) %>%
      dplyr::left_join(truth, by = c("location"="location","week_match"="week")) %>%
      dplyr::filter((as.numeric(bin)<=weighted_ili) & (as.numeric(bin_end)>weighted_ili)) %>%
      dplyr::mutate(logscore=log(as.numeric(value)))} 
  else {
    scored_entry <- entry %>%
      dplyr::mutate(week_match=as.numeric(substr(target,start=1,stop=1))+forecast_week,
                    bin_end=ifelse(as.numeric(bin)!=25,as.numeric(bin)+0.1,100)) %>%
      dplyr::left_join(truth, by = c("location"="region","week_match"="week")) %>%
      dplyr::filter((as.numeric(bin)<=unweighted_ili) & (as.numeric(bin_end)>unweighted_ili)) %>%
      dplyr::mutate(logscore=log(as.numeric(value)))
  }
  return(scored_entry)
}