#' Compute the sequence of dates in CDCEpi format from the start date to the end date
#'
#' @param submission_df Full submission file as dataframe from either inf code or read_entry
#' @return A plot list of two faceted plots, one for week targets and one for seasonal
#' @export
#' @keywords internal
#' @seealso \code{\link{get_viz_from_submission_df}}
get_viz_from_submission_df <- function(submission_df){
  #construct density plots for 1:6
  p_1_6 <- ggplot(submission_df[submission_df$target %in% paste0(1:6, " wk ahead"),],aes(x=as.numeric(bin),y=as.numeric(value))) + geom_line()+ facet_wrap(location~target,scales = "free" ) + ylab("Density") + xlab("ILI")
  # contsruct density plots for seasonal
  p_seasonal <- ggplot(submission_df[!(submission_df$target %in% paste0(1:6, " wk ahead")),],aes(x=bin,y=as.numeric(value),group=bin)) + geom_point()+ facet_wrap(location~target,scales = "free" ) + ylab("Density") + xlab("ILI")
  # plot_list
  plot_list <- list()
  plot_list[[1]] <- p_1_6
  plot_list[[1]] <- p_seasonal
  
  return (plot_list)
}