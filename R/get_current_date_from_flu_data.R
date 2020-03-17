get_current_date_from_flu_data <- function(flu_data){
  return (paste0(tail(flu_data$year,1),"-EW", ifelse(nchar(tail(flu_data$week,1))==2,tail(flu_data$week,1),paste0("0",tail(flu_data$week,1)))))
}