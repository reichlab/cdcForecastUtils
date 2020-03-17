context("numeric samples to binned distributions")
library(dplyr)
library(cdcForecastUtils)

test_that("numeric_samples_to_binned_distribution works", {
  bins <- c(seq(0, 9.9, by=0.1), 100)
  x <- c(1.1, 1.15, 1.199999, 1.2, 3.3, 74.2)
  
  actual <- numeric_samples_to_binned_distribution(x, bins)
  
  expected <- data.frame(
    bin = bins[seq_len(length(bins) - 1)],
    value = 0
  )
  expected$value[abs(expected$bin - 1.1) < 0.001] <- 3/6
  expected$value[abs(expected$bin - 1.2) < 0.001] <- 1/6
  expected$value[abs(expected$bin - 3.3) < 0.001] <- 1/6
  expected$value[abs(expected$bin - 9.9) < 0.001] <- 1/6
  
  expect_identical(actual, expected)
})

test_that("numeric_samples_to_binned_distribution fails with x outside bins", {
  bins <- c(seq(0, 9.9, by=0.1), 100)
  x <- c(1.1, 1.15, 1.199999, 1.2, 3.3, 100.1)
  
  expect_error(numeric_samples_to_binned_distribution(x, bins))
})
