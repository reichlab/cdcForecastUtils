context("generate_point_forecasts")

test_that("Correct entries are successful.",{
  generate_point_forecasts(no_point_entry_new)
  generate_point_forecasts(no_point_entry_state_new, challenge = "state_ili")
})

test_that("Point forecasts exist report warning.", {
  generate_point_forecasts(full_entry_new)
  generate_point_forecasts(full_entry_state_new, challenge = "state_ili")
})
