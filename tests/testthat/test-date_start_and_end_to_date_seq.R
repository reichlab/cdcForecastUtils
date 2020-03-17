context("get_time_from_start_of_season")

test_that("Test season start time",{
  expect_true(get_time_from_start_of_season("2020-EW10","2020-EW35")==26)
})


