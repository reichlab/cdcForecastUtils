#' Compute the time from the start of season
#'
#' @param date_start Start of the date sequence
#' @param current_time End of the date sequence
#' @return integer of time between date_start and current_time
#' @export
#' @keywords internal
#' @seealso \code{\link{get_time_from_start_of_season}}


get_time_from_start_of_season <-
function(season_start,current_time){
  date_seq <- date_start_and_end_to_date_seq(season_start,current_time)
  return (length(date_seq))
}
