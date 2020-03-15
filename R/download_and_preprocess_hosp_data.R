download_and_preprocess_hosp_data <-
function(latest_year = as.numeric(format(Sys.Date(), "%Y"))) {
  flu_data_raw_hosp <-cdcfluview::hospitalizations(years=2009:latest_year)
  flu_data_raw_hosp$week <- flu_data_raw_hosp$year_wk_num
  flu_data <- dplyr::mutate(flu_data_raw_hosp,
    time = as.POSIXct(MMWRweek::MMWRweek2Date(year, week)))
  
  ## set rows with denominator zeroes to NAs
  #flu_data[which(flu_data$total_patients==0),"weighted_ili"] <- NA
  
  ## Add time_index column: the number of days since some origin date
  ## (1970-1-1 in this case).  The origin is arbitrary.
  flu_data$time_index <- as.integer(lubridate::date(flu_data$time) - lubridate::ymd("1970-01-01"))
  
  ## Season column: for example, weeks of 2010 up through and including week 30
  ## get season 2009/2010; weeks after week 30 get season 2010/2011
  ## Official CDC flu season for the purposes of prediction runs from week 40 of
  ## one year to week 20 of the next; the season start week we define here is the
  ## mid-point of the "off-season"
  flu_data$season <- ifelse(
    flu_data$week <= 30,
    paste0(flu_data$year - 1, "/", flu_data$year),
    paste0(flu_data$year, "/", flu_data$year + 1)
  )
  
  ## Season week column: week number within season
  ## weeks after week 30 get season_week = week - 30
  ## weeks before week 30 get season_week = week + (number of weeks in previous year) - 30
  ## This computation relies on the start_date function in package MMWRweek,
  ## which is not exported from that package's namespace!!!
  flu_data$season_week <- ifelse(
    flu_data$week <= 30,
    flu_data$week + MMWRweek::MMWRweek(MMWRweek:::start_date(flu_data$year) - 1)$MMWRweek - 30,
    flu_data$week - 30
  )
  
  hosp_flu <- as.data.frame(flu_data)
  
  return(hosp_flu)
}
