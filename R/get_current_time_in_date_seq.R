#' Get index of epidemic week within a sequence of epidemic weeks
#' 
#' @param current_time character: epidemic week in format "2020-EW10"
#' @param date_seq character: sequence of epidemic weeks in format "2020-EW10"
#' 
#' @return integer index of current time within date sequence
#' 
#' @export
get_current_time_in_date_seq <-
function(current_time, date_seq) {
  return (which(date_seq==current_time))
}
