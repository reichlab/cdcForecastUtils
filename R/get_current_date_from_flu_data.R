#' Get current epidemic week from flu data
#' 
#' @param flu_data data frame with year and week columns giving MMWR year and week
#' 
#' @return epidemic week in format "2020-EW11"
#' 
#' @export
get_current_date_from_flu_data <- function(flu_data){
  return (paste0(tail(flu_data$year,1),"-EW", ifelse(nchar(tail(flu_data$week,1))==2,tail(flu_data$week,1),paste0("0",tail(flu_data$week,1)))))
}