#' Compute the sequence of dates in CDCEpi format from the start date to the end date
#'
#' @param date_start Start of the date sequence
#' @param date_end End of the date sequence
#' @return Array of dates between date_start and date_end in CDCEpi format
#' @export
#' @keywords internal
#' @seealso \code{\link{date_start_and_end_to_date_seq}}
date_start_and_end_to_date_seq <- function(date_start,date_end){
  year_start <- as.numeric(substr(date_start,1,4))
  week_start <- as.numeric(substr(date_start,8,10))
  
  year_end <- as.numeric(substr(date_end,1,4))
  week_end <- as.numeric(substr(date_end,8,10))
  
  date_sequence <- seq(MMWRweek::MMWRweek2Date(MMWRyear = year_start,MMWRweek = week_start),
                       MMWRweek::MMWRweek2Date(MMWRyear = year_end,MMWRweek = week_end), by="weeks")
  back_to_dates <- MMWRweek::MMWRweek(date_sequence)
  back_to_dates$MMWRweek <- unlist(lapply(back_to_dates$MMWRweek,function(x){
    if (nchar(x)==1){
      return (paste0("0",x))
    } else {
      return (x)
    }
  }))
  
  return(paste0(back_to_dates$MMWRyear,"-EW",back_to_dates$MMWRweek))
}