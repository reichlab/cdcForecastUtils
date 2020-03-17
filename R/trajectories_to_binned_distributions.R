#' Convert a matrix of sampled trajectories to binned distributions for
#' short-term and/or seasonal targets.
#' 
#' @param trajectories matrix of simulated trajectories.  Each row is one
#'    simulated trajectory, each column is one time point.  Must contain all
#'    time points needed to calculate seasonal and short-term targets
#' @param targets: character vector specifying targets to compute. May include:
#'    "wk ahead", "Below baseline for 3 weeks", "First week below baseline",
#'    "Peak height", "Peak week"
#' @param h_max largest horizon for short-term targets
#' @param bins: vector of start and end points for incidence targets.
#'    For example: c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0)
#' @param baseline: baseline for this unit, if applicable; required if
#'    "onset timing", "Below baseline for 3 weeks", or
#'    "First week below baseline" are among the targets to forecast
#' @param season_start_ew: character specification of season start epidemic
#'    week, formatted as "2019-EW40"
#' @param season_end_ew: character specification of season end epidemic week,
#'    formatted as "2020-EW20"
#' @param cdc_report_ew: character specification of epidemic week corresponding
#'    to most recent cdc report, formatted as "2019-EW40"
#' 
#' @return data frame with columns:
#'    target: with values coming from targets
#'    type: populated with "Bin"
#'    bin: name of bin for categorical targets, lower endpoint of bin for
#'        numeric targets
#'    value: proportion of trajectories falling in bin
#' 
#' @export
trajectories_to_binned_distributions <-
function(
  trajectories,
  targets,
  h_max,
  bins,
  baseline,
  season_start_ew,
  season_end_ew,
  cdc_report_ew
)
{
  # validate targets
  valid_targets <- c("wk ahead", "Below baseline for 3 weeks",
    "First week below baseline", "Peak height", "Peak week")
  valid_targets_lower <- tolower(valid_targets)
  user_targets <- match.arg(tolower(targets), valid_targets_lower, several.ok = TRUE)
  if(length(user_targets) < length(targets)) {
    warning(paste0("Unsupported target requested; targets must be one or more of: ",
              paste(valid_targets, collapse = ", ")))
  }
  targets <- valid_targets[valid_targets_lower %in% user_targets]
  
  # set up globals
  date_seq <- date_start_and_end_to_date_seq(season_start_ew,season_end_ew)
  idx_of_current_time <- get_current_time_in_date_seq(cdc_report_ew,date_seq)
  
  # validate trajectory lengths
  # minimum length as determined by baseline targets
  baseline_targets <- c("Below baseline for 3 weeks", "First week below baseline")
  if(any(baseline_targets %in% targets)) {
    min_length_a <- length(date_seq) + 2
  } else {
    min_length_a <- length(date_seq)
  }
  
  # minimum length as determined by week ahead targets
  # e.g. if number of weeks in season is 27 and idx_of_current_time is 25 we need 27 + 4
  if("wk ahead" %in% targets) {
    min_length_b <- length(date_seq) + h_max - (length(date_seq) - idx_of_current_time)
  } else {
    min_length_b <- length(date_seq)
  }
  min_length <- max(min_length_a, min_length_b)
  
  if(ncol(trajectories) < min_length) {
    stop("trajectories does not have enough columns for the requested targets.  Must provide at least h_max weeks after cdc_report_ew for wk ahead target and 2 weeks after season_end_ew for baseline targets.")
  }
  
  # wk ahead
  if("wk ahead" %in% targets) {
    short_term_results <- purrr::map_dfr(
      seq(idx_of_current_time+1,idx_of_current_time+h_max),
      function(h) {
        numeric_samples_to_binned_distribution(
          x = trajectories[, h],
          bins = bins) %>%
          dplyr::mutate(
            target = paste0(h-idx_of_current_time, " wk ahead"),
            type = "bin"
          )
      }
    )
  } else {
    short_term_results <- NULL
  }
  
  # targets related to baseline
  if("Below baseline for 3 weeks" %in% targets ||
     "First week below baseline" %in% targets) {
    if(missing(baseline)) {
      stop("Requested target involving baseline, but baseline not provided.")
    }
    
    if(ncol(trajectories) < length(date_seq)+3) {
      stop("Requested target involving baseline, but did not provide long enough trajectories.")
    }
    
    trajectories_for_baseline_calc <- trajectories[, seq_len(length(date_seq) + 2), drop = FALSE]
    below_baseline_idx_by_trajectory <- apply(
      trajectories_for_baseline_calc,
      1,
      get_below_baseline_idx,
      baseline = baseline,
      lookahead_length = 3L
    )
    
    # below baseline
    if("Below baseline for 3 weeks" %in% targets) {
      below_baseline <- data.frame(
        target = "Below baseline for 3 weeks",
        type = "bin",
        bin = "true",
        value = mean(!is.na(below_baseline_idx_by_trajectory)),
        stringsAsFactors = FALSE
      )
    } else {
      below_baseline <- NULL
    }
    
    # first week below baseline
    non_na_idx <- below_baseline_idx_by_trajectory[!is.na(below_baseline_idx_by_trajectory)]
    if("First week below baseline" %in% targets && length(non_na_idx) > 0) {
      first_below_baseline <- categorical_samples_to_binned_distribution(
        date_seq[non_na_idx],
        date_seq
      ) %>%
        dplyr::mutate(
          target = "First week below baseline",
          type = "bin"
        )
    } else {
      first_below_baseline <- NULL
    }
  } else {
    below_baseline <- NULL
    first_below_baseline <- NULL
  }
  
  # peak height
  if("Peak height" %in% targets) {
    season_peak_height <- numeric_samples_to_binned_distribution(
        x = rowMax(trajectories),
        bin = bins) %>%
      dplyr::mutate(
        target = "Peak height",
        type = "bin"
      )
  } else {
    season_peak_height <- NULL
  }
  
  # peak week
  if("Peak week" %in% targets) {
    season_peak_week <- numeric_samples_to_binned_distribution(
        x = rowMaxWeek(trajectories),
        bin = seq(1,length(date_seq)+1)) %>%
      dplyr::mutate(
        target = "Peak week",
        type = "bin"
      )
    
    season_peak_week$bin <- date_seq[1:(length(date_seq))]
  } else {
    season_peak_week <- NULL
  }

  # assemble and return
  submission_df <- rbind(
    short_term_results,
    season_peak_week,
    season_peak_height,
    below_baseline,
    first_below_baseline)

  return(submission_df)
}
