context("verify_point")

test_that("Valid entry passes", {
  expect_true(verify_point(full_entry_new))
  expect_true(verify_point(full_entry_state_new))
})

test_that("Missing point predictions return warning", {
  rand_target <- sample(unique(full_entry_new$target), 1)
  rand_location <- sample(unique(full_entry_new$location), 1)
  
  invalid_full <- full_entry_new
  invalid_full$value[invalid_full$location == rand_location & 
                       invalid_full$target == rand_target &
                       invalid_full$type == "point"] <- NA
  
  expect_warning(verify_point(invalid_full))

})

test_that("Negative point prediction return error", {
  rand_target <- sample(unique(full_entry_new$target), 1)
  rand_location <- sample(unique(full_entry_new$location), 1)
  
  invalid_full <- full_entry_new
  invalid_full$value[invalid_full$location == rand_location & 
                       invalid_full$target == rand_target &
                       invalid_full$type == "point"] <- -1
  
  expect_error(verify_point(invalid_full))
  
})