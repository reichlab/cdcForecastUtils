source('utilities.R')

#' From a vector of numeric samples, create data frame of bin probabilities.
#'
#' @param x numeric vector of samples
#' @param bins: numeric vector of bin start and end points.  For example:
#'    c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0)
#'
#' @return Data frame with columns:
#'    bin_start_incl: lower endpoints of bins
#'    bin_end_notincl: upper endpoints of bins
#'    value: proportion of x falling in bin
#'
#' @export
numeric_samples_to_binned_distribution <- function(
  x,
  bins) {
  # counts in each bin
  bin_counts <- hist(x, bins, right = FALSE, plot = FALSE)$counts
  
  # results data frame
  num_bins <- length(bins) - 1
  return(
    data.frame(
      bin_start_incl = bins[seq_len(num_bins)],
      bin_end_notincl = bins[seq_len(num_bins) + 1],
      value = bin_counts / sum(bin_counts),
      stringsAsFactors = FALSE
    )
  )
}



#' From vector of categorical samples, create data frame of bin probabilities.
#' 
#' @param x character vector of samples
#' @param bins character vector of bin values.
#' 
#' @return Data frame with columns:
#'    bin_start_incl: lower endpoints of bins
#'    bin_end_notincl: upper endpoints of bins
#'    value: proportion of x falling in bin
#'
#' @export
categorical_samples_to_binned_distribution <- function(
  x,
  bins
) {
  # counts in each bin
  bin_counts <- sapply(bins, function(bin) {sum(bins == x, na.rm = TRUE)})
  
  # results data frame
  num_bins <- length(bins) - 1
  return(
    data.frame(
      bin_start_incl = bins,
      bin_end_notincl = bins,
      value = bin_counts / sum(bin_counts),
      stringsAsFactors = FALSE
    )
  )
}

#' Convert a matrix of sampled trajectories to binned distributions for
#' short-term and seasonal targets.
#' 
#' @param trajectories nsim by weeks in season matrix of simulated trajectories
#' @param h_max integer, largest horizon to predict for short term targets
#' @param bins numeric vector of bin start and end points.  For example:
#'    c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0)
#' @param round_digits integer; if provided,
#'    round(trajectories, digits = round_digits) is called before binning.
#' 
#' @return data frame with columns:
#'    target: like “1 wk ahead”, “2 wk ahead”, etc.
#'    type: populated with "Bin"
#'    bin_start_incl: lower endpoints of bins
#'    bin_end_notincl: upper endpoints of bins
#'    value: proportion of trajectories falling in bin
#' 
#' @export
trajectories_to_short_term_and_seasonal_binned_distributions <- function(
  trajectories,
  h_max,
  bins,
  season_start,
  season_end,
  current_time,
  nsim,
  round_digits
)
{
  # set up globals
  date_seq <-date_start_and_end_to_date_seq(season_start,season_end)
  idx_of_current_time <- get_current_time_in_date_seq(current_time,date_seq)
    
  short_term_results <- purrr::map_dfr(
    seq(idx_of_current_time+1,idx_of_current_time+h_max),
    function(h) {
      numeric_samples_to_binned_distribution(
        x = trajectories[, h],
        bins = bins) %>%
        mutate(
          target = paste0(h-idx_of_current_time, " wk ahead"),
          type = "Bin"
        )
    }
  )
  
  
  season_peak_percentage <- purrr::map_dfr(
    1,
    function(traj_idx) {
      numeric_samples_to_binned_distribution(
        x = rowMax(trajectories),
        bins = bins) %>%
        mutate(
          target = "Peak Percentage",
          type = "Bin"
        )
    }
  )
  season_peak_week <- purrr::map_dfr(
    1,
    function(traj_idx) {
      numeric_samples_to_binned_distribution(
        x = rowMaxWeek(trajectories),
        bins = seq(1,length(date_seq))) %>%
        mutate(
          target = "Peak Week",
          type = "Bin"
        )
    }
  )
  season_peak_week$bin_start_incl <- date_seq[1:(length(date_seq)-1)]
  season_peak_week$bin_end_notincl <- date_seq[2:(length(date_seq))]
  
  submission_df <- rbind(short_term_results,season_peak_week,season_peak_percentage)
  
  return (submission_df)
  
}


### unit test

unit_test_results <- trajectories_to_short_term_and_seasonal_binned_distributions(trajectories = matrix(rep(1:20,100),nrow=100,byrow = T),
                                                             h_max = 6,
                                                             bins = c(seq(0,13,by=.1),100),
                                                             season_start = "2018-EW40",
                                                             season_end= "2019-EW20",
                                                             current_time = "2018-EW42",
                                                             nsim=100)
                                                            




#############################
#### DEPRECATED
####################

#' Convert a matrix of sampled trajectories to binned distributions for
#' short-term targets.
#' 
#' @param trajectories nsim by h matrix of simulated trajectories
#' @param h_max integer, largest horizon to predict
#' @param bins numeric vector of bin start and end points.  For example:
#'    c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0)
#' @param round_digits integer; if provided,
#'    round(trajectories, digits = round_digits) is called before binning.
#' 
#' @return data frame with columns:
#'    target: like “1 wk ahead”, “2 wk ahead”, etc.
#'    type: populated with "Bin"
#'    bin_start_incl: lower endpoints of bins
#'    bin_end_notincl: upper endpoints of bins
#'    value: proportion of trajectories falling in bin
#' 
#' @export
trajectories_to_short_term_binned_distributions <- function(
  trajectories,
  h_max,
  bins,
  round_digits
) {
  if(ncol(trajectories) < h_max) {
    stop("trajectories has fewer columns than the largest requested horizon.")
  }
  
  results <- purrr::map_dfr(
    seq_len(h_max),
    function(h) {
      numeric_samples_to_binned_distribution(
        x = trajectories[, i],
        bins = bins) %>%
        mutate(
          target = paste0(h, " wk ahead"),
          type = "Bin"
        )
    }
  )
  
  return(results)
}



#' Convert a matrix of sampled trajectories to binned distributions for
#' seasonal targets.
#' 
#' @param trajectories nsim by h matrix of simulated trajectories
#' @param season_start_ew: integer, starting MMWR epidemic week for seasonal
#'    targets, inclusive
#' @param season_start_year: integer, starting year for seasonal targets,
#'    inclusive (needed to handle 53 week years)
#' @param season_end_ew: integer, ending MMWR epidemic week for seasonal
#'    targets, inclusive
#' @param season_end_year: integer, ending year for seasonal targets, inclusive
#' @param observed_data: data frame with at minimum columns:
#'    epidemic_week
#'    year
#'    target variable being forecasted
#' @param target_variable: character name of column in observed data that is
#'    being forecasted
#' @param bins: named list of bin start and end points.  For example:
#'    list(
#'      "season peak incidence" = c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0),
#'      "season peak timing" = c(seq(from = 40, to = 53), seq(from = 1, to = 20)),
#'      "onset timing" = c(as.character(c(seq(from = 40, to = 53), seq(from = 1, to = 20))), "none")
#' @param round_digits: named list of targets for which to do rounding and
#'    number of digits to round.  For example:
#'    list("onset timing" = 1) to do rounding only for onset timing
#' 
#' @return data frame with columns:
#'    target: with values coming from names(bins)
#'    type: populated with "Bin"
#'    bin_start_incl: lower endpoints of bins
#'    bin_end_notincl: upper endpoints of bins
#'    value: proportion of trajectories falling in bin
#' 
#' @export
trajectories_to_seasonal_binned_distributions <- function(
  
) {
  # to do: parameter validation
  
  
  # to do: Add data from season_start week to present to beginning of trajectories
  
  
  results <- purrr::map_dfr(
    names(bins),
    function(target) {
      # to do: Calculate target value for each sampled trajectory
      target_values_by_trajectory <- NA
      
      # Get bin probabilities
      if(is.numeric(bins[[target]])) {
        results <- numeric_samples_to_binned_distribution(
          x = target_values_by_trajectory,
          bins = bins)
      } else if(is.character(bins[[target]])) {
        results <- categorical_samples_to_binned_distribution(
          x = target_values_by_trajectory,
          bins = bins
        )
      }
      
      results <- results %>%
        mutate(
          target = target,
          type = "Bin"
        )
      
      return(results)
    }
  )
  
  return(results)
}






    
