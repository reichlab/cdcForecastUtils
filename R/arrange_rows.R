#' Arrange the rows of the file
#'
#' Restructure rows based on the template.
#'
#' @param file A submission file
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'    challenge the submission is for
#' @return An arranged data.frame
#' @import dplyr
#' @export
arrange_rows = function(entry,challenge= "ilinet") {
  if(challenge== "ilinet"){template <- cdcForecastUtils::full_entry_new}
  if(challenge== "state_ili"){template <- cdcForecastUtils::full_entry_state_new}
  entry_location_target <- subset(template, (location %in% entry$location) & (target %in% entry$target)) %>%
    dplyr::select(-"value")
  entry_arrangedRow <- entry_location_target %>%
    dplyr::left_join(entry, by=c("location"="location","target"="target",
                                 "type"="type","bin"="bin"))
  if(nrow(entry_arrangedRow)==nrow(entry)){
    final_entry<-entry_arrangedRow
  } else {
    final_entry<-entry
    message("Rows cannot be arranged. Original entry is returned")
  }
  return(final_entry)
}