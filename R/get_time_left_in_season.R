#' @export
get_time_left_in_season <-
function(current_time,season_end){
  date_seq <- date_start_and_end_to_date_seq(current_time,season_end)
  # THIS IS PROBABLY WRONG 
  return (max(length(date_seq)+2,6))
}
