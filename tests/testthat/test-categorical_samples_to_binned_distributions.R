context("categorical_samples_to_binned_distribution")
library(dplyr)
library(cdcForecastUtils)


test_that("categorical_samples_to_binned_distribution works, character inputs", {
  x <- c("a", "b", "b", "c", "a", "a")
  bins <- letters[1:4]
  
  actual <- categorical_samples_to_binned_distribution(x, bins)
  
  expected <- data.frame(
    bin = letters[1:4],
    value = (3:0)/6,
    stringsAsFactors = FALSE
  )
  rownames(expected) <- bins
  
  expect_equal(actual, expected)
})


test_that("categorical_samples_to_binned_distribution works, factor inputs", {
  x <- factor(c("a", "b", "b", "c", "a", "a"))
  bins <- factor(letters[1:4])
  
  actual <- categorical_samples_to_binned_distribution(x, bins)
  
  expected <- data.frame(
    bin = letters[1:4],
    value = (3:0)/6,
    stringsAsFactors = FALSE
  )
  rownames(expected) <- bins
  
  expect_equal(actual, expected)
})


test_that("categorical_samples_to_binned_distribution if observed categories not in bins", {
  x <- c("a", "b", "b", "c", "a", "a")
  bins <- letters[1:2]
  
  expect_error(categorical_samples_to_binned_distribution(x, bins))
})
