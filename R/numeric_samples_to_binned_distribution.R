numeric_samples_to_binned_distribution <-
function(
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
