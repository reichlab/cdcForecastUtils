context("verify_probabilities")


test_that("Valid entry passes", {
  expect_true(verify_probabilities(full_entry_new))
  expect_true(verify_probabilities(full_entry_state_new))
})


test_that("Missing probabilities throw errors", {
  rand_target <- sample(unique(full_entry_new$target), 1)
  rand_location <- sample(unique(full_entry_new$location), 1)
  
  invalid_min <- full_entry_new
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "bin"] <- NA
  invalid_full <- full_entry_new
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "bin"] <- NA
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})

test_that("Negative probabilities throw errors", {
  rand_target <- sample(unique(full_entry_new$target), 1)
  rand_location <- sample(unique(full_entry_new$location), 1)

  invalid_full <- full_entry_new
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "bin"] <- -0.1
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})

test_that("Probabilities summing to < 0.9 throw errors", {
  rand_target <- sample(unique(full_entry_new$target), 1)
  rand_location <- sample(unique(full_entry_new$location), 1)
  
  invalid_full <- full_entry_new
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "bin"] <- 0.01
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})

test_that("Probabilities summing to > 1.1 throw errors", {
  rand_target <- sample(unique(full_entry_new$target), 1)
  rand_location <- sample(unique(full_entry_new$location), 1)

  invalid_full <- full_entry_new
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "bin"] <- 0.1
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})

test_that("Binary probabilities higher than 1 throw errors", {
  rand_target <- "Below baseline for 3 weeks"
  rand_location <- sample(unique(full_entry_new$location), 1)
  
  invalid_full <- full_entry_new
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "bin"] <- 1.1
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})