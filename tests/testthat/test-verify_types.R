context("verify_types")

test_that("Correct entries are successful.",{
  expect_true(verify_types(full_entry_new))
  expect_true(verify_types(full_entry_state_new, challenge = "state_ili"))
  expect_true(verify_types(hosp_template, challenge = "hospitalization"))
})

test_that("Missing types report errors.", {
  valid_types <- unique(full_entry_new$type)
  
  for (i in seq_along(valid_types)) {
    tmp_entry <- full_entry_new[full_entry_new$type != valid_types[i],]
    expect_error(verify_types(tmp_entry))
  }
})

test_that("Extra type reports warning.", {
  tmp_entry <- full_entry_new
  tmp_entry$type[1] <- "extra type"
  expect_warning(verify_types(tmp_entry))
})