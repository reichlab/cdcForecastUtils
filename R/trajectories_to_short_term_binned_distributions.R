trajectories_to_short_term_binned_distributions <-
function(
  trajectories,
  h_max,
  bins,
  season_start,
  season_end,
  current_time,
  round_digits
)
{
  
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
  
}
