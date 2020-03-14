context("verify_locations")

test_that("Correct entries are successful.",{
  expect_true(verify_locations(full_entry_new))
  expect_true(verify_locations(full_entry_state_new, "state_ili"))
})

test_that("Wrong challenge specification reports errors.", {
  expect_error(verify_locations(full_entry_state_new, "ilinet"))
})

test_that("Extra location reports warning.", {
  tmp_entry <- full_entry_new
  tmp_entry$location[1] = "extra location"
  expect_warning(verify_locations(tmp_entry))
  tmp_entry_state <- full_entry_state_new
  tmp_entry_state$location[1] = "extra location"
  expect_warning(verify_locations(tmp_entry_state, "state_ili"))
})

test_that("Missing regional location reports message.", {
  tmp_entry <- full_entry_new
  region_locations = setdiff(unique(full_entry_new$location), "US National")
  for (i in seq_along(region_locations)) {
    tmp_entry <- full_entry_new[full_entry_new$location != region_locations[i],]
    expect_message(verify_locations(tmp_entry))
  }
})