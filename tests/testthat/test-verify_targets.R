
context("verify_targets")

test_that("Correct entries are successful.",{
  expect_true(verify_targets(full_entry_new))
  expect_true(verify_targets(full_entry_state_new, challenge = "state_ili"))
  expect_true(verify_targets(hosp_template, challenge = "hospitalization"))
  
})

test_that("Missing targets report message.", {
  valid_targets <- unique(full_entry_new$target)
  for (i in seq_along(valid_targets)) {
    tmp_entry <- full_entry_new[full_entry_new$target != valid_targets[i],]
    expect_message(verify_targets(tmp_entry))
  }
})

test_that("Extra target reports warning.", {
  tmp_entry <- full_entry_new
  tmp_entry$target[1] <- "extra target"
  expect_warning(verify_targets(tmp_entry))
})