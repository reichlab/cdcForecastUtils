#' Fetch and combine data from the Delphi Epidata API
#' 
#' @param source currently, one of "fluview", "twitter", or "wiki"
#' @param regions character vector of region codes
#' @param issues issues to fetch, e.g. c(201040, 201041, 201042); used only for fluview
#' @param epiweeks_range range of epiweeks to fetch, e.g. c(201040, 201530)
#' 
#' @return data frame with all results from Epidata
#' 
#' @export
fetch_delphi_data_multi_issue <-
function(
  source = c("fluview", "twitter", "wiki"),
  regions = "nat",
  issues = 1,
  epiweeks_range) {
  
  source <- match.arg(source, choices = c("fluview", "twitter", "wiki"))
  
  epiweeks <- list(Epidata$range(epiweeks_range[1], epiweeks_range[2]))
  
  all_obs <- purrr::map_dfr(regions, function(region) {
    purrr::map_dfr(issues, function(issue) {
      if(identical(source, "fluview")) {
        obs_one_issue <- Epidata$fluview(
          regions = list(region),
          epiweeks = epiweeks,
          issue = list(issue))
      } else if(identical(source, "twitter")) {
        obs_one_issue <- Epidata$twitter(
          locations = list(region),
          epiweeks = epiweeks
        )
      } else if(identical(source, "wiki")) {
        obs_one_issue <- Epidata$wiki(
          articles = list("influenza", "common_cold", "cough"),
          language = "en",
          epiweeks = epiweeks
        )
      } else {
        stop("Unsupported Epidata source")
      }
      
      temp <- purrr::map_dfr(obs_one_issue$epidata,
                             function(x) {
                               x[sapply(x, function(comp) is.null(comp))] <- NA
                               return(as.data.frame(x))
                             })
      
      if(identical(source, "wiki")) {
        temp <- temp %>%
          dplyr::select(article, epiweek, value) %>%
          dplyr::spread(article, value) %>%
          dplyr::rename(
            wiki_influenza = influenza,
            wiki_common_cold = common_cold,
            wiki_cough = cough
          )
      }
      
      return(temp)
    })
  })
  
  all_obs <- all_obs %>%
    separate(epiweek, c("year", "week"), sep=4, remove=FALSE) %>%
    mutate(
      year = as.integer(year),
      week = as.integer(week))
  
  return(all_obs)
}
