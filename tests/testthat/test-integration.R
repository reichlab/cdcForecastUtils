library(cdcForecastUtils)


test_that("test_full_integration on dummy df for ", {
  bins <-  c(seq(0, 25, by = .1))
  dummy_submission <- data.frame(target="1 wk ahead",bin=bins,value=c(1,rep(0,length(bins)-1)),type='bin',location='dummy')
  traj_list <-list()
  traj_list[[1]] <- matrix(rep(c(1,rep(0,6))),nrow=100,ncol=7,byrow = T)
   
  dummy_trajectory <- data.frame(location="dummy")
  dummy_trajectory$trajectories <- traj_list
  sub_df <-multi_trajectories_to_binned_distributions(
    multi_trajectories = dummy_trajectory,
    targets = "wk ahead",
    h_max = 6,
    bins = c(seq(0, 25, by = .1), 100),
    season_start_ew = season_start,
    season_end_ew = season_end,
    cdc_report_ew = current_date_in_cdc_format)
  
  expect_equal(sum(data.frame(sub_df[sub_df$target == "1 wk ahead",]$value) ==dummy_submission$value ),nrow(dummy_submission))
  expect_equal(sum(data.frame(sub_df[sub_df$target == "1 wk ahead",]$bin) ==dummy_submission$bin ),nrow(dummy_submission))
  
})
  