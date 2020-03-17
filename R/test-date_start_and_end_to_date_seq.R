context("date_start_and_end_to_date_seq")

test_that("Test date sequences",{
  expect_true(verify_bins(full_entry_new))
  expect_true(verify_bins(full_entry_state_new, challenge = "state_ili"))
})


test_that("Missing bins report errors.", {
  rand_target <- sample(full_entry_new$target, 1)
  
  valid_bins <- unique(full_entry_new$bin[full_entry_new$target ==
                                            rand_target & 
                                            full_entry_new$type == 
                                            "bin"])
  
  for (i in seq_along(valid_bins)) {
    tmp_entry <- full_entry_new[full_entry_new$bin != valid_bins[i], ]
    expect_error(verify_bins(tmp_entry))
  }
})

test_that("Extra bin reports warning.", {
  rand_target <- sample(full_entry_new$target, 1)
  
  extra_row <- head(full_entry_new[full_entry_new$target == rand_target &
                                     full_entry_new$type == "bin", ], n = 1)
  extra_row$bin <- "extra"
  tmp_entry <- rbind(full_entry_new, extra_row)
  
  expect_warning(verify_bins(tmp_entry))
})

