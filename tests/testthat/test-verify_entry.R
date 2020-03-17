context("verify_entry")

valid_file <- system.file("extdata/EW10-2019-valid_national_template.csv", package="cdcForecastUtils")
valid_entry <- cdcForecastUtils::read_entry(valid_file)
valid_state_file <- system.file("extdata/EW10-2019-valid_state_template.csv", package = "cdcForecastUtils")
valid_state_entry <- cdcForecastUtils::read_entry(valid_state_file)

invalid_file_col <- system.file("extdata/EW01-2020-national_invalidCols.csv", package="cdcForecastUtils")

test_that("Valid entry passes", {
  expect_true(verify_entry_file(valid_file))
  expect_true(verify_entry(valid_entry))
  expect_true(verify_entry_file(valid_state_file, challenge = "state_ili"))
  expect_true(verify_entry(valid_state_entry, challenge = "state_ili"))
})

# test_that("Entry without forecast week generates warnings", {
#   expect_warning(verify_entry_file(valid_file))
#   expect_warning(verify_entry(valid_entry))
# })


test_that("Return error when required column name doesn't exist", {
  rand_column <- sample(names(valid_entry), 1)
  invalid_entry <- valid_entry
  names(invalid_entry)[names(invalid_entry) == rand_column] <- "invalidName"
  expect_error(arrange_entry(invalid_entry))
  expect_error(verify_entry( invalid_entry))
})


test_that("Return error when probabilities are missing", {
  rand_location <- sample(unique(valid_entry$location), 1)
  rand_target <- sample(unique(valid_entry$target), 1)
  
  invalid_entry <- valid_entry
  invalid_entry$value[invalid_entry$location == rand_location &
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "bin"] <- NA
  
  expect_error(verify_probabilities(invalid_entry))
  expect_error(verify_entry(invalid_entry))
  
})


test_that("Return error when probabilities are negative", {
  rand_location <- sample(unique(valid_entry$location), 1)
  rand_target <- sample(unique(valid_entry$target), 1)
  
  invalid_entry <- valid_entry
  invalid_entry$value[invalid_entry$location == rand_location &
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "bin"] <- -0.5
  
  expect_error(verify_probabilities(invalid_entry))
  expect_error(verify_entry(invalid_entry))
  
})


test_that("Return error when probabilities sum to less than 0.9", {
  rand_location <- sample(unique(valid_entry$location), 1)
  rand_target <- sample(unique(valid_entry$target), 1)
  
  invalid_entry <- valid_entry
  invalid_entry$value[invalid_entry$location == rand_location &
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "bin"] <- 0.01
  
  expect_error(verify_probabilities(invalid_entry))
  expect_error(verify_entry(invalid_entry))
  
  
})


test_that("Return error when probabilities sum to more than 1.1", {
  rand_location <- sample(unique(valid_entry$location), 1)
  rand_target <- sample(unique(valid_entry$target), 1)
  
  invalid_entry <- valid_entry
  invalid_entry$value[invalid_entry$location == rand_location &
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "bin"] <- 0.1
  
  expect_error(verify_probabilities(invalid_entry))
  expect_error(verify_entry(invalid_entry))
  
})

test_that("Return warning when point forecast is missing", {
  rand_location <- sample(unique(valid_entry$location), 1)
  rand_target <- sample(unique(valid_entry$target), 1)
  
  invalid_entry <- valid_entry
  invalid_entry$value[invalid_entry$location == rand_location &
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "point"] <- NA
  
  expect_warning(verify_point(invalid_entry))
  expect_warning(verify_entry(invalid_entry))
  
})

test_that("Return error when point forecast is negative", {
  rand_location <- sample(unique(valid_entry$location), 1)
  rand_target <- sample(unique(valid_entry$target), 1)
  
  invalid_entry <- valid_entry
  invalid_entry$value[invalid_entry$location == rand_location &
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "point"] <- -1
  expect_error(verify_point(invalid_entry))
  expect_error(verify_entry(invalid_entry))
  
})