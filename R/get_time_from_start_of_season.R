get_time_from_start_of_season <-
function(season_start,current_time){
  date_seq <- date_start_and_end_to_date_seq(season_start,current_time)
  return (length(date_seq))
}
