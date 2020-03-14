context("verify_colnames")

test_that("Correct entries are successful.",{
  expect_true(verify_colnames(full_entry_score_new))
})

test_that("Missing columns report errors.", {
  for (i in seq_along(colnames(full_entry_score_new)[1:5])) 
    expect_error(verify_colnames(full_entry_score_new[,-i]))
})

test_that("Missing forecast_week column report warning.", {
    expect_warning(verify_colnames(full_entry_score_new[,-6]))
})
# test_that("Missing forecast_week reports warning", {
#   expect_warning(verify_colnames(minimal_entry))
#   expect_warning(verify_colnames(full_entry))
# })

test_that("Extra column report warnings.", {
  tmp_entry <- full_entry_score_new
  tmp_entry$extra_column = NA
  expect_warning(verify_colnames(tmp_entry))
})