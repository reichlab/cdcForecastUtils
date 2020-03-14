#' @export
generate_csv_from_submission_df <- function(submission_df,path){
  current_date <- submission_df$forecast_week[1]
  write.csv(submission_df, file =paste0(path,current_date,".csv"))
  
}