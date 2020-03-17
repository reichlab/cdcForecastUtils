#' Convert a collection of matrices of sampled trajectories for different units
#' (e.g., different locations or age groups) to binned distributions for
#' short-term and/or seasonal targets.
#' 
#' @param multi_trajectories tibble with at minimum a column called
#'    `trajectories` containing simulated trajectories.   Other
#'    columns may also be provided which are treated as uniquely
#'    identifying units such as locations or age groups.
#' @param targets: character vector specifying targets to compute. May include:
#'    "wk ahead", "Below baseline for 3 weeks", "First week below baseline",
#'    "Peak height", "Peak week"
#' @param h_max largest horizon for short-term targets
#' @param bins: vector of start and end points for incidence targets.
#'    For example: c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0)
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
#'    bin: name of bin for categorical targets, lower endpoint of bin for
#'        numeric targets
#'    value: proportion of trajectories falling in bin
#' 
#' @export
multi_trajectories_to_binned_distributions <- function(
  multi_trajectories,
  targets,
  h_max,
  bins,
  season_start_ew,
  season_end_ew,
  cdc_report_ew
) {
  if ("First week below baseline" %in% targets | "Below baseline for 3 weeks" %in% targets){
    return_df <- data.frame()
    for (location in multi_trajectories$location){
      local_baseline <- get_ili_baseline(location,year=2015)
      tmp_df <- trajectories_to_binned_distributions(multi_trajectories[multi_trajectories$location == location,]$trajectories[[1]],
                                           targets = targets,
                                           baseline = local_baseline,
                                           h_max = h_max,
                                           bins = bins,
                                           season_start_ew = season_start_ew,
                                           season_end_ew = season_end_ew,
                                           cdc_report_ew = cdc_report_ew)                        
      tmp_df$location <- location
      return_df <- rbind(return_df,tmp_df)
    }
    return (return_df)
  } else{
  return(
    multi_trajectories %>%
      dplyr::mutate(
        summaries = purrr::map(
          trajectories,
          trajectories_to_binned_distributions,
          baseline=FALSE,
          targets = targets,
          h_max = h_max,
          bins = bins,
          season_start_ew = season_start_ew,
          season_end_ew = season_end_ew,
          cdc_report_ew = cdc_report_ew
        )
      ) %>%
      dplyr::select(-trajectories) %>%
      tidyr::unnest(cols = summaries)
  )
  }
}
