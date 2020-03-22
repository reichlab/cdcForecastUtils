#' Verifies appropriate filename structure
#'
#' @param filename a string representing a forecast entry filename
#' @param challenge string indicating which project the submission is for
#'
#' @return
#' @export
#'
verify_filename <- function(filename, challenge){
    
    if (!(challenge %in% c("ilinet", "state_ili"))) {
        stop("verifying filename is only supported for ilinet or state_ili projects")
    }
    
    message("checking that filename matches 'YYYY-ewZZ-teamabbr-modelabbr.csv' format.")
    ## expected filename for ilinet and state_ili is the following format:
    ##  YYYY-ewWW-teamabbr-modelabbr.csv
    
    ## check that fileame has only basename
    if(basename(filename) != filename) 
        stop("please ensure that the filename does not have any directories in it.")
    
    ## split filename up
    name_segments <- unlist(strsplit(filename, "[-.]"))
    
    ## check that there are exactly three hyphens
    if(length(unlist(strsplit(filename, "-"))) != 4)
        stop("please ensure that every required filename element is separated by a hyphen and that there are no other hypens in your filename.")
    
    ## check 4-digit year >= 2020
    if(is.na(as.numeric(name_segments[1])) | nchar(as.numeric(name_segments[1]))!=4)
        stop("filename should start with a 4-digit number.")
    if(as.numeric(name_segments[1])<2020)
        stop("filename should start with a year on or after 2020.")
        
    ## check that `ew` is present
    if(!grepl("^ew[0-9]{2}$", name_segments[2]))
        stop("please ensure that your filename represents the week with lowercase ew followed by two digits.")
    
    ## check two digit week btw 01 and 53
    week_num <- as.numeric(substr(name_segments[2], 3, 4))
    if(week_num < 1 | week_num > 53)
        stop("please check that week number is between 1 and 53.")
    
    ## check that team abbr is <20 char
    if(nchar(name_segments[3])>20)
        warning("please consider renaming your team abbreviation to be <20 charaters.")
    
    ## check that team abbr is alphanumeric + _
    if(!grepl("^[a-zA-Z0-9_]*$", name_segments[3]))
        stop("team abbreviation must only contain alphanumeric characters and underscores.")
    
    ## check that model abbr is <20 char
    if(nchar(name_segments[4])>20)
        warning("please consider renaming your model abbreviation to be <20 charaters.")

    ## check that model abbr is alphanumeric + _
    if(!grepl("^[a-zA-Z0-9_]*$", name_segments[4]))
        stop("model abbreviation must only contain alphanumeric characters and underscores.")
    
    ## check that there is `.csv`
    if(name_segments[5]!="csv")
        stop("please confirm that you are submitting a .csv file.")
    
    return(TRUE)
}