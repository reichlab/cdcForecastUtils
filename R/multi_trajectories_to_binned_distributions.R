#' Convert a collection of matrices of sampled trajectories for different units
#' (e.g., different locations or age groups) to binned distributions for
#' short-term and/or seasonal targets.
#' 
#' @param multi_trajectories tibble with at minimum a column called
#'    `trajectories` containing simulated trajectories.   Other
#'    columns may also be provided which are treated as uniquely
#'    identifying units such as locations or age groups.
#' @param h_max largest horizon for short-term targets
#' @param bins: named list of bin start and end points.  For example:
#'    list(
#'      "season peak incidence" = c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0),
#'      "season peak timing" = c(seq(from = 40, to = 53), seq(from = 1, to = 20)),
#'      "onset timing" = c(as.character(c(seq(from = 40, to = 53), seq(from = 1, to = 20))), "none")
#' @param season_start_ew: character specification of season start epidemic
#'    week, formatted as "2019-EW40"
#' @param season_end_ew: character specification of season end epidemic week,
#'    formatted as "2020-EW20"
#' @param cdc_report_ew: character specification of epidemic week corresponding
#'    to most recent cdc report, formatted as "2019-EW40"
#' 
#' @return data frame with columns:
#'    ...: columns in multi_trajectories other than `trajectories`
#'    target: with values coming from names(bins)
#'    type: populated with "Bin"
#'    bin_start_incl: lower endpoints of bins
#'    bin_end_notincl: upper endpoints of bins
#'    value: proportion of trajectories falling in bin
#' 
#' @export
multi_trajectories_to_binned_distributions <- function(
  multi_trajectories,
  h_max,
  bins,
  season_start_ew,
  season_end_ew,
  cdc_report_ew
) {
  return(
    multi_trajectories %>%
      dplyr::mutate(
        summaries = purrr::map(
          trajectories,
          trajectories_to_binned_distributions,
          h_max = h_max,
          bins = bins,
          season_start_ew = season_start_ew,
          season_end_ew = season_end_ew,
          cdc_report_ew = cdc_report_ew
        )
      ) %>%
      select(-trajectories) %>%
      tidyr::unnest(cols = summaries)
  )
}
