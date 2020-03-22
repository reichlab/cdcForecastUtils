context("verify_filename")

valid_filename <- "2020-ew34-JDU-DLSpecialSauce.csv"

invalid_filenames <- c(
  "202-ew34-JDU-DLSpecialSauce.csv", ## wrong year
  "2020-EW34-JDU-DLSpecialSauce.csv", ## EW not ew
  "2020-ew3-JDU-DLSpecialSauce.csv", ## 2 digit week
  "2020-ew34-JD&U-DLSpecialSauce.csv" ## non-alphanumeric in team
)



test_that("Valid entry filename passes", {
  expect_true(verify_filename(valid_filename, challenge="ilinet"))
  expect_true(verify_filename(valid_filename, challenge="state_ili"))
})

test_that("Invalid entry filenames do not pass", {
  for(invalid_filename in invalid_filenames) {
    expect_failure(verify_filename(invalid_filename, challenge="ilinet"))
    expect_failure(verify_filename(invalid_filename, challenge="state_ili"))
  }
})
