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
  
  expect_warning(verify_point(invalid_full))
  
})

test_that("Out of range point prediction for week targets return error", {
  rand_target <- sample(full_entry_new %>% 
                          dplyr::filter(target %in% c("Peak week","First week below baseline")),1)
  rand_location <- sample(unique(full_entry_new$location), 1)
  
  invalid_full <- full_entry_new %>% 
    dplyr::filter(target %in% c("Peak week","First week below baseline"))
  invalid_full$value[invalid_full$location == rand_location & 
                       invalid_full$target == rand_target &
                       invalid_full$type == "point"] <- "2020-EW43"
  
  expect_warning(verify_point(invalid_full))
  
})

test_that("Incorrect point prediction format for week targets return error", {
  rand_target <- sample(full_entry_new %>% 
                          dplyr::filter(target %in% c("Peak week","First week below baseline")),1)
  rand_location <- sample(unique(full_entry_new$location), 1)
  
  invalid_full <- full_entry_new %>% 
    dplyr::filter(target %in% c("Peak week","First week below baseline"))
  invalid_full$value[invalid_full$location == rand_location & 
                       invalid_full$target == rand_target &
                       invalid_full$type == "point"] <- "2019-EW43"
  
  expect_warning(verify_point(invalid_full))
  
})