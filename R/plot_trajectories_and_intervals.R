#' Plot trajectories and intervals
#' @param flu_data A dataframe housing the flu data from the result of 
#'     download_and_preprocess...
#' @param target_variable 
#' @param trajectories_by_location A series of trajectories (nsim*h) per location
#' @param submission the submission dataframe
#' @param season_start_ew
#' @param season_end_ew
#' @param cdc_report_ew
#' @return A plot
#' @import magrittr
#' @export

plot_trajectories_and_intervals <- function(
  flu_data,
  target_variable,
  trajectories_by_location,
  submission,
  season_start_ew,
  season_end_ew,
  cdc_report_ew
) {
  flu_data_to_plot <- flu_data %>%
    mutate(location = region) %>%
    filter(location %in% unique(submission$location)) %>%
    group_by(location) %>%
    top_n(30, time)
  
  
  trajectories_df <- trajectories_by_location %>%
    purrr::pmap_dfr(
      function(location, trajectories) {
        purrr::map_dfr(
          seq_len(nrow(trajectories)),
          function(i) {
            data.frame(
              location = location,
              trajectory_ind = i,
              time = as.POSIXct(
                7*(seq_len(ncol(trajectories))-1) + MMWRweek::MMWRweek2Date(MMWRyear = 2020, MMWRweek = 10)
              ),
              simulated_incidence = trajectories[i, ],
              stringsAsFactors = FALSE
            )
          }
        )
      }
    )
  
  ps <- c(0.5, 0.005, 0.995, 0.025, 0.975, 0.10, 0.90, 0.25, 0.75)
  sample_summaries <- submission %>%
    filter(type == "bin") %>%
    group_by(location, target) %>%
    do(
      setNames(
        binned_distribution_quantile(bins = .[["bin"]], bin_probabilities = .[["value"]], p = ps) %>%
          matrix(nrow = 1) %>%
          as.data.frame(stringsAsFactors = FALSE),
        paste0("q", ps)
      )
    )
  
  short_term_summaries <- sample_summaries %>%
    filter(grepl("wk ahead", target, fixed = TRUE)) %>%
    mutate(
      time = as.POSIXct(
        7 * sapply(target, function(t) {as.numeric(substr(t, 1, 1))}) +
          MMWRweek::MMWRweek2Date(
            MMWRyear = as.numeric(substr(cdc_report_ew, 1, 4)),
            MMWRweek = as.numeric(strsplit(cdc_report_ew, "ew")[[1]][2])
          )
      )
    ) %>%
    mutate_at(vars(starts_with("q")), as.numeric)
  
  peak_height_summaries <- expand.grid(
    time = sort(unique(c(flu_data_to_plot$time, trajectories_df$time))),
    location = unique(flu_data_to_plot$location)
  ) %>%
    left_join(
      sample_summaries %>% filter(target == "Peak height"),
      by = "location"
    ) %>%
    mutate_at(vars(starts_with("q")), as.numeric)
  
  
  peak_week_summaries <- sample_summaries %>%
    filter(target == "Peak week") %>%
    tidyr::pivot_longer(cols = starts_with("q"), names_to = "quantile", names_prefix = "q", values_to = "time") %>%
    mutate(
      quantile = as.numeric(quantile),
      time = as.POSIXct(
        purrr::map_chr(
          time,
          function(time) {
            year <- as.numeric(substr(time, 1, 4))
            ew <- as.numeric(strsplit(time, "ew")[[1]][2])
            MMWRweek::MMWRweek2Date(
              MMWRyear = year,
              MMWRweek = ew
            ) %>% as.character
          }
        )
      )
    ) %>%
    left_join(
      trajectories_df %>%
        group_by(location) %>%
        summarize(
          ymax = max(simulated_incidence)
        ),
      by = "location"
    )
  
  if("First week below baseline" %in% sample_summaries$target) {
    baseline_summaries <- sample_summaries %>%
      filter(target == "First week below baseline") %>%
      tidyr::pivot_longer(cols = starts_with("q"), names_to = "quantile", names_prefix = "q", values_to = "time") %>%
      mutate(
        quantile = as.numeric(quantile),
        time = as.POSIXct(
          purrr::map_chr(
            time,
            function(time) {
              year <- as.numeric(substr(time, 1, 4))
              ew <- as.numeric(strsplit(time, "ew")[[1]][2])
              MMWRweek::MMWRweek2Date(
                MMWRyear = year,
                MMWRweek = ew
              ) %>% as.character
            }
          )
        )
      ) %>%
      left_join(
        trajectories_df %>%
          group_by(location) %>%
          summarize(
            ymax = max(simulated_incidence)
          ),
        by = "location"
      )
    
    baselines <- data.frame(
      location = unique(sample_summaries$location),
      baseline = sapply(
        unique(sample_summaries$location),
        function(location) {
          cdcForecastUtils::get_ili_baseline(location = location, year = 2019)
        })
    )
  }
  
  temp <- pretty(c(trajectories_df$simulated_incidence, flu_data_to_plot$weighted_ili))
  ylim <- c(temp[1], tail(temp, 1))
  
  traj_alpha <- 0.1
  interval_alpha <- 0.2
  
  p <- ggplot() +
    geom_line(
      data = trajectories_df,
      mapping = aes(x = time, y = simulated_incidence, group = trajectory_ind, color = "Simulated Forecast"), alpha = traj_alpha) +
    geom_line(
      data = flu_data_to_plot,
      mapping = aes(x = time, y = get(target_variable), group = region, color = "Observed")) +
    geom_line(data = short_term_summaries, mapping = aes(x = time, y = q0.5, color = "Simulated Forecast")) +
    geom_vline(
      data.frame(
        xintercept = as.POSIXct(MMWRweek::MMWRweek2Date(
          MMWRyear = as.numeric(substr(season_start_ew, 1, 4)),
          MMWRweek = as.numeric(strsplit(season_start_ew, "ew")[[1]][2])
        ))
      ),
      mapping = aes(xintercept = xintercept, color = "Season Boundaries")) +
    geom_vline(
      data.frame(
        xintercept = as.POSIXct(MMWRweek::MMWRweek2Date(
          MMWRyear = as.numeric(substr(season_end_ew, 1, 4)),
          MMWRweek = as.numeric(strsplit(season_end_ew, "ew")[[1]][2])
        ))
      ),
      mapping = aes(xintercept = xintercept, color = "Season Boundaries")) +
    geom_ribbon(data = short_term_summaries, mapping = aes(x = time, ymin = q0.005, ymax = q0.995, fill = "wk ahead"), alpha = interval_alpha) +
    geom_ribbon(data = short_term_summaries, mapping = aes(x = time, ymin = q0.025, ymax = q0.975, fill = "wk ahead"), alpha = interval_alpha) +
    geom_ribbon(data = short_term_summaries, mapping = aes(x = time, ymin = q0.1, ymax = q0.9, fill = "wk ahead"), alpha = interval_alpha) +
    geom_ribbon(data = short_term_summaries, mapping = aes(x = time, ymin = q0.25, ymax = q0.75, fill = "wk ahead"), alpha = interval_alpha) +
    geom_ribbon(data = peak_height_summaries, mapping = aes(x = time, ymin = q0.005, ymax = q0.995, fill = "Peak height"), alpha = interval_alpha) +
    geom_ribbon(data = peak_height_summaries, mapping = aes(x = time, ymin = q0.025, ymax = q0.975, fill = "Peak height"), alpha = interval_alpha) +
    geom_ribbon(data = peak_height_summaries, mapping = aes(x = time, ymin = q0.1, ymax = q0.9, fill = "Peak height"), alpha = interval_alpha) +
    geom_ribbon(data = peak_height_summaries, mapping = aes(x = time, ymin = q0.25, ymax = q0.75, fill = "Peak height"), alpha = interval_alpha) +
    geom_area(data = peak_week_summaries %>% filter(quantile %in% c(0.005, 0.995)),
              mapping = aes(x = time, y = ymax, fill = "Peak week"), alpha = interval_alpha) +
    geom_area(data = peak_week_summaries %>% filter(quantile %in% c(0.025, 0.975)),
              mapping = aes(x = time, y = ymax, fill = "Peak week"), alpha = interval_alpha) +
    geom_area(data = peak_week_summaries %>% filter(quantile %in% c(0.1, 0.9)),
              mapping = aes(x = time, y = ymax, fill = "Peak week"), alpha = interval_alpha) +
    geom_area(data = peak_week_summaries %>% filter(quantile %in% c(0.25, 0.75)),
              mapping = aes(x = time, y = ymax, fill = "Peak week"), alpha = interval_alpha) +
    ylab("ILI") +
    facet_wrap( ~ location, scales = "free_y", ncol = 1) +
    theme_bw()
  
  if("First week below baseline" %in% sample_summaries$target) {
    p <- p +
      geom_hline(data = baselines, mapping = aes(yintercept = baseline, color = "Baseline"), linetype = 2) +
      geom_area(data = baseline_summaries %>% filter(quantile %in% c(0.005, 0.995)),
                mapping = aes(x = time, y = ymax, fill = "First week\nbelow baseline"), alpha = interval_alpha) +
      geom_area(data = baseline_summaries %>% filter(quantile %in% c(0.025, 0.975)),
                mapping = aes(x = time, y = ymax, fill = "First week\nbelow baseline"), alpha = interval_alpha) +
      geom_area(data = baseline_summaries %>% filter(quantile %in% c(0.1, 0.9)),
                mapping = aes(x = time, y = ymax, fill = "First week\nbelow baseline"), alpha = interval_alpha) +
      geom_area(data = baseline_summaries %>% filter(quantile %in% c(0.25, 0.75)),
                mapping = aes(x = time, y = ymax, fill = "First week\nbelow baseline"), alpha = interval_alpha)
  }
  
  print(p)
  
  return(invisible(p))
}