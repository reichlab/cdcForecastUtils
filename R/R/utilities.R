############################
### UTILITIES FOR DATES
#####################
library(MMWRweek)
rowMax <- function(trajectories){
  retarr <- apply(trajectories,1,max)
  return(retarr)
}

rowMaxWeek <- function(trajectories){
  retarr <- apply(trajectories,1,which.max)
  return(retarr)
}


date_start_and_end_to_date_seq <- function(date_start,date_end){
  year_start <- as.numeric(substr(date_start,1,4))
  week_start <- as.numeric(substr(date_start,8,10))
  
  year_end <- as.numeric(substr(date_end,1,4))
  week_end <- as.numeric(substr(date_end,8,10))
  
  date_sequence <- seq(MMWRweek2Date(MMWRyear = year_start,MMWRweek = week_start),
      MMWRweek2Date(MMWRyear = year_end,MMWRweek = week_end), by="weeks")
  back_to_dates <- MMWRweek(date_sequence)
  back_to_dates$MMWRweek <- unlist(lapply(back_to_dates$MMWRweek,function(x){
    if (nchar(x)==1){
      return (paste0("0",x))
    }else{
      return (x)
    }
  }))
  return (paste0(back_to_dates$MMWRyear,"-EW",back_to_dates$MMWRweek))
  
}

    ## unit test

date_start <- "2018-EW40"
date_end <- "2019-EW20"
date_start_and_end_to_date_seq(date_start,date_end)


get_current_time_in_date_seq <- function(current_time,date_seq){
  return (which(date_seq==current_time))
}

get_time_left_in_season <- function(current_time,season_end){
  date_seq <- date_start_and_end_to_date_seq(current_time,season_end)
  return (length(date_seq))
}

get_time_from_start_of_season <- function(season_start,current_time){
  date_seq <- date_start_and_end_to_date_seq(season_start,current_time)
  return (length(date_seq))
}
