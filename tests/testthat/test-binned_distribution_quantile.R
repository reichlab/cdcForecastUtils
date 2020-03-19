context("categorical_samples_to_binned_distribution")
library(dplyr)
library(cdcForecastUtils)

test_that("binned_distribution_quantile works", {
  bins <- c(seq(from = 0.0, to = 9.9, by = 0.1), 100.0)
  bin_probabilities <- rep(0.01, 100)
  p <- c(0.005, 0.015, 0.1, 0.7, 0.99)
  expected <- bins[c(1, 2, 10, 70, 99)]
  
  actual <- binned_distribution_quantile(
    bins = bins,
    bin_probabilities = bin_probabilities,
    p = p)
  
  expect_identical(actual, expected)
})
