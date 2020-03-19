#' Sanitize a csv entry file
#'
#' This function re-format entry files for minor formatting issues.
#'
#' @param entry A data.frame of a csv entry
#' @return A correctly formatted data.frame
#' @export
sanitize_entry <- function(entry){
  # change all column names to lower case
  names(entry) <- tolower(names(entry))
  
  # get rid of blank spaces on the left+right sides in locations and targets
  entry$location <- trimws(entry$location, which="both")
  entry$target <- trimws(entry$target, which="both")
  
  # change type to lower case
  entry$type <- tolower(trimws(entry$type, which="both"))
  
  # sanitize bins
  # add .0 to 1-25 integer bins
  if(length(entry$bin[which(entry$type=="bin" & (grepl("wk ahead",entry$target) | grepl("Peak height",entry$target)) & 
             (!is.na(entry$bin)) & (nchar(entry$bin) <=2))])>0){
    entry$bin[which(entry$type=="bin" & (grepl("wk ahead",entry$target) | grepl("Peak height",entry$target)) & 
                      (!is.na(entry$bin)) & (nchar(entry$bin) <=2))] <- paste0(entry$bin,".0")  
  } 
  # trim white spaces in week targets and change to lower case
  if(length(entry$bin[which(entry$type=="bin" & 
                    (grepl("First week below baseline",entry$target) | grepl("Below baseline for 3 weeks",entry$target)) & 
                    (!is.na(entry$bin)))])>0){
    entry$bin[which(entry$type=="bin" & 
                      (grepl("First week below baseline",entry$target) | grepl("Below baseline for 3 weeks",entry$target)) & 
                                                             (!is.na(entry$bin)))] <- tolower(trimws(entry$bin, which="both"))}  
    
  # sanitize value
  entry$value <- tolower(trimws(entry$value, which="both"))
  
  return (entry)
}