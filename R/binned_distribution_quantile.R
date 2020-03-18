#' Find quantiles of a binned distribution
#' 
#' @param bins vector of lower endpoints of bins (possibly also including upper
#'    endpoint of last bin), assumed to be in increasing order of bins
#' @param bin_probabilities numeric vector of probabilities for all bins,
#'    assumed to be in increasing order of bins
#' @param p vector of probabilities giving quantiles to find
#' 
#' @return numeric vector of lower endpoints of bins in which the specified
#'    quantiles fall
#' 
#' @export
binned_distribution_quantile <- function(
  bins,
  bin_probabilities,
  p)
{
  cum_probs <- cumsum(bin_probabilities)
  quantile_inds <- purrr::map_int(p, function(one_p) {
    min(which(cum_probs >= one_p))
  })
  
  return(bins[quantile_inds])
}
