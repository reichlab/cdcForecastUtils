context("generate_point_forecasts")

test_that("Point forecasts exist report warning.", {
  expect_warning(generate_point_forecasts(full_entry_new))
  expect_warning(generate_point_forecasts(full_entry_state_new))
})


