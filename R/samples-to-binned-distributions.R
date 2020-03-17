
#' From a vector of numeric samples, create data frame of bin probabilities.
#'
#' @param x numeric vector of samples
#' @param bins: numeric vector of bin start and end points.  For example:
#'    c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0)
#'
#' @return Data frame with columns:
#'    bin: lower endpoints of bins
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
      bin = bins[seq_len(num_bins)],
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
#'    bin: bin name
#'    value: proportion of x falling in bin
#'
#' @export
categorical_samples_to_binned_distribution <- function(
  x,
  bins
) {
  # convert everything to character
  x <- as.character(x)
  bins <- as.character(bins)
  if(!all(x %in% bins)) {
    stop("Sample values x are not all in bins")
  }
  
  # counts in each bin
  bin_counts <- sapply(
    bins,
    function(bin) {
      sum(x == bin, na.rm = TRUE)
    }
  )
  
  # results data frame
  num_bins <- length(bins) - 1
  return(
    data.frame(
      bin = bins,
      value = bin_counts / sum(bin_counts),
      stringsAsFactors = FALSE
    )
  )
}

### unit test

# unit_test_results <- trajectories_to_short_term_and_seasonal_binned_distributions(trajectories = matrix(rep(1:20,100),nrow=100,byrow = T),
#                                                              h_max = 6,
#                                                              bins = c(seq(0,13,by=.1),100),
#                                                              season_start = "2018-EW40",
#                                                              season_end= "2019-EW20",
#                                                              current_time = "2018-EW42",
#                                                              nsim=100)
