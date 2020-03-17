context("trajectories to binned distributions")
library(dplyr)
library(cdcForecastUtils)

test_that("trajectories_to_binned_distributions: \"Below baseline for 3 weeks\" and \"First week below baseline\", all above baseline", {
  # single trajectory, all above baseline
  baseline <- 1.0
  trajectories <- matrix(
    rep(baseline + 0.01, 53),
    nrow = 1
  )
  
  expected <- data.frame(
    target = "Below baseline for 3 weeks",
    type = "bin",
    bin = "true",
    value = 0.0,
    stringsAsFactors = FALSE
  )
  
  actual <- trajectories_to_binned_distributions(
    trajectories = trajectories,
    baseline = baseline,
    targets = c("Below baseline for 3 weeks", "First week below baseline"),
    season_start_ew = "2020-EW10",
    season_end_ew = "2020-EW40",
    cdc_report_ew = "2020-EW10")
  
  expect_identical(expected, actual)
  
  
  
  # multiple trajectories, all above baseline
  baseline <- 1.0
  trajectories <- cbind(
    matrix(rep(baseline + 0.01, 53), nrow = 1),
    matrix(rep(baseline + 1.01, 53), nrow = 1)
  )
  
  expected <- data.frame(
    target = "Below baseline for 3 weeks",
    type = "bin",
    bin = "true",
    value = 0.0,
    stringsAsFactors = FALSE
  )
  
  actual <- trajectories_to_binned_distributions(
    trajectories = trajectories,
    baseline = baseline,
    targets = c("Below baseline for 3 weeks", "First week below baseline"),
    season_start_ew = "2020-EW10",
    season_end_ew = "2020-EW40",
    cdc_report_ew = "2020-EW10")
  
  expect_identical(expected, actual)
})



test_that("trajectories_to_binned_distributions: \"Below baseline for 3 weeks\" and \"First week below baseline\", below baseline less than 3 weeks", {
  # single trajectory, below baseline for 2 weeks then back up
  baseline <- 1.0
  trajectories <- matrix(
    rep(baseline + 0.01, 53),
    nrow = 1
  )
  trajectories[1, 4:5] <- baseline - 0.01
  
  expected <- data.frame(
    target = "Below baseline for 3 weeks",
    type = "bin",
    bin = "true",
    value = 0.0,
    stringsAsFactors = FALSE
  )
  
  actual <- trajectories_to_binned_distributions(
    trajectories = trajectories,
    baseline = baseline,
    targets = c("Below baseline for 3 weeks", "First week below baseline"),
    season_start_ew = "2020-EW10",
    season_end_ew = "2020-EW40",
    cdc_report_ew = "2020-EW10")
  
  expect_identical(expected, actual)
  
  
  
  # multiple trajectories, all above baseline
  baseline <- 1.0
  trajectories <- cbind(
    matrix(rep(baseline + 0.01, 53), nrow = 1),
    matrix(rep(baseline + 1.01, 53), nrow = 1)
  )
  trajectories[1, 4:5] <- baseline - 0.01
  trajectories[1, 14:15] <- baseline - 0.01
  
  expected <- data.frame(
    target = "Below baseline for 3 weeks",
    type = "bin",
    bin = "true",
    value = 0.0,
    stringsAsFactors = FALSE
  )
  
  actual <- trajectories_to_binned_distributions(
    trajectories = trajectories,
    baseline = baseline,
    targets = c("Below baseline for 3 weeks", "First week below baseline"),
    season_start_ew = "2020-EW10",
    season_end_ew = "2020-EW40",
    cdc_report_ew = "2020-EW10")
  
  expect_identical(expected, actual)
})



test_that("trajectories_to_binned_distributions: \"Below baseline for 3 weeks\" and \"First week below baseline\", below baseline 3 weeks, 52 week year", {
  season_start_ew <- "2020-EW10"
  season_end_ew <- "2020-EW40"
  cdc_report_ew <- "2020-EW10"
  
  # single trajectory, below baseline for 3 weeks then back up
  baseline <- 1.0
  trajectories <- matrix(
    rep(baseline + 0.01, 53),
    nrow = 1
  )
  target_week <- 4
  trajectories[1, target_week:(target_week + 2)] <- baseline - 0.01
  
  expected <- rbind(
    data.frame(
      target = "Below baseline for 3 weeks",
      type = "bin",
      bin = "true",
      value = 1.0,
      stringsAsFactors = FALSE
    ),
    data.frame(
      target = "First week below baseline",
      type = "bin",
      bin = paste0("2020-EW", 10:40),
      value = c(
        rep(0.0, target_week - 1),
        1.0,
        rep(0.0, (length(10:40) - target_week))
      )
    )
  )
  
  actual <- trajectories_to_binned_distributions(
    trajectories = trajectories,
    baseline = baseline,
    targets = c("Below baseline for 3 weeks", "First week below baseline"),
    season_start_ew = "2020-EW10",
    season_end_ew = "2020-EW40",
    cdc_report_ew = "2020-EW10")
  
  expect_identical(expected, actual)
  
  
  
  # multiple trajectories, below baseline for 3 weeks then back up
  baseline <- 1.0
  trajectories <- rbind(
    matrix(rep(baseline, 53), nrow = 1),
    matrix(rep(baseline, 53), nrow = 1),
    matrix(rep(baseline, 53), nrow = 1)
  )
  target_weeks <- c(4, 14, 7)
  trajectories[1, target_weeks[1]:(target_weeks[1] + 2)] <- baseline - 0.01
  trajectories[2, target_weeks[2]:ncol(trajectories)] <- baseline - 0.01
  trajectories[3, target_weeks[3]:ncol(trajectories)] <- baseline - 0.01
  
  expected <- rbind(
    data.frame(
      target = "Below baseline for 3 weeks",
      type = "bin",
      bin = "true",
      value = 1.0,
      stringsAsFactors = FALSE
    ),
    data.frame(
      target = "First week below baseline",
      type = "bin",
      bin = paste0("2020-EW", 10:40),
      value = rep(0.0, length(10:40))
    )
  )
  expected$value[expected$bin %in% paste0("2020-EW", target_weeks + 9)] <- 1/3
  
  actual <- trajectories_to_binned_distributions(
    trajectories = trajectories,
    baseline = baseline,
    targets = c("Below baseline for 3 weeks", "First week below baseline"),
    season_start_ew = "2020-EW10",
    season_end_ew = "2020-EW40",
    cdc_report_ew = "2020-EW10")
  
  expect_identical(expected, actual)
})



test_that("trajectories_to_binned_distributions: \"Below baseline for 3 weeks\" and \"First week below baseline\", below baseline 3 weeks, 52 week year", {
  season_start_ew <- "2020-EW10"
  season_end_ew <- "2020-EW40"
  cdc_report_ew <- "2020-EW10"
  
  # multiple trajectories, below baseline for 3 weeks then back up
  baseline <- 1.0
  trajectories <- rbind(
    matrix(rep(baseline, 53), nrow = 1),
    matrix(rep(baseline, 53), nrow = 1),
    matrix(rep(baseline, 53), nrow = 1),
    matrix(rep(baseline, 53), nrow = 1)
  )
  target_weeks <- c(4, 14, 7)
  trajectories[1, target_weeks[1]:(target_weeks[1] + 2)] <- baseline - 0.01
  trajectories[2, target_weeks[2]:ncol(trajectories)] <- baseline - 0.01
  trajectories[3, target_weeks[3]:ncol(trajectories)] <- baseline - 0.01
  
  expected <- rbind(
    data.frame(
      target = "Below baseline for 3 weeks",
      type = "bin",
      bin = "true",
      value = 0.75,
      stringsAsFactors = FALSE
    ),
    data.frame(
      target = "First week below baseline",
      type = "bin",
      bin = paste0("2020-EW", 10:40),
      value = rep(0.0, length(10:40))
    )
  )
  expected$value[expected$bin %in% paste0("2020-EW", target_weeks + 9)] <- 1/3
  
  actual <- trajectories_to_binned_distributions(
    trajectories = trajectories,
    baseline = baseline,
    targets = c("Below baseline for 3 weeks", "First week below baseline"),
    season_start_ew = "2020-EW10",
    season_end_ew = "2020-EW40",
    cdc_report_ew = "2020-EW10")
  
  expect_identical(expected, actual)
})

