#' Compute the sequence of dates in CDCEpi format from the start date to the end date
#'
#' @param submission_df Full submission file as dataframe from either inf code or read_entry
#' @return A plot list unique to targets
#' @export
#' @keywords internal
#' @seealso \code{\link{get_viz_from_submission_df}}
get_viz_from_submission_df <- function(submission_df){
  submission_df_no_point_forecasts <- submission_df[submission_df$type=="bin",]
  targets <- unique(submission_df_no_point_forecasts$target)
  plot_list = list()
  for (i in 1:length(targets)) {
    if (targets[i] %in% c(paste0(1:6," wk ahead"))){
      p <- ggplot(submission_df_no_point_forecasts[submission_df_no_point_forecasts$target == targets[i],],aes(x=as.numeric(bin),y=as.numeric(value),group=1)) + geom_line(size=1)+ facet_wrap(location~target,scales = "free" ) + ylab("Density") + xlab("ILI") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else{
      p <- ggplot(submission_df_no_point_forecasts[submission_df_no_point_forecasts$target == targets[i],],aes(x=bin,y=as.numeric(value),group=bin)) + geom_point(size=1)+ facet_wrap(location~target,scales = "free" ) + ylab("Density") + xlab("ILI") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    plot_list[[i]] = p
  }
  

  return (plot_list)
}