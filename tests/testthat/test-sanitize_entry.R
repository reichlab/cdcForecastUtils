context("sanitize_entry")

filepath_need_sanitizing <- system.file("extdata/example_submission_df_for_unit_test.csv",package="cdcForecastUtils")
file_need_sanitizing <- read.csv(filepath_need_sanitizing,
                                 colClasses = "character",
                                 stringsAsFactors = FALSE)
file_need_sanitizing <- file_need_sanitizing[,-c(1)]
test_that("Sanitizing works if files match", {
  expect_equivalent(sanitize_entry(file_need_sanitizing),sanitized_state)
  })

