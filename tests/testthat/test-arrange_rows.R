context("verify_entry")

valid_file <- system.file("extdata/EW10-2019-valid_national_template.csv",package="cdcForecastUtils")
valid_entry <- read_entry(valid_file)
valid_state_file <- system.file("extdata/EW10-2019-valid_state_template.csv", package = "cdcForecastUtils")
valid_state_entry <- read_entry(valid_state_file)

test_that("Valid entry passes", {
  expect_equivalent(arrange_rows(valid_entry),cdcForecastUtils::full_entry_new)
  expect_equivalent(arrange_rows(valid_state_entry,challenge="state_ili"),cdcForecastUtils::full_entry_state_new)
})
