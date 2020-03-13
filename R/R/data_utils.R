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
fetch_delphi_data_multi_issue <- function(
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


#' Download and preprocess the latest CDC flu data, both national and regional
#'
#' @param latest_year year through which data should be downloaded, defaults to current year
#'
#' @return data frame with latest flu data, preprocessed
#' @export
download_and_preprocess_flu_data <- function(latest_year = as.numeric(format(Sys.Date(), "%Y"))) {
  require(cdcfluview)
  require(lubridate)
  require(dplyr)
  require(MMWRweek)
  
  regionflu <- ilinet(region="hhs", years= 1997:latest_year)
  regionflu$region <- as.character(regionflu$region)
  
  usflu <- ilinet(region="national", years= 1997:latest_year)
  
  flu_data <- bind_rows(regionflu, usflu)
  
  ## set rows with denominator zeroes to NAs
  flu_data[which(flu_data$total_patients==0),"weighted_ili"] <- NA
  
  flu_data <- transmute(flu_data,
                        region_type = region_type,
                        region = as.factor(region),
                        year = year,
                        week = week,
                        time = as.POSIXct(MMWRweek2Date(year, week)),
                        weighted_ili = weighted_ili)
  
  ## Add time_index column: the number of days since some origin date
  ## (1970-1-1 in this case).  The origin is arbitrary.
  flu_data$time_index <- as.integer(lubridate::date(flu_data$time) -  ymd("1970-01-01"))
  
  ## Season column: for example, weeks of 2010 up through and including week 30
  ## get season 2009/2010; weeks after week 30 get season 2010/2011
  ## Official CDC flu season for the purposes of prediction runs from week 40 of
  ## one year to week 20 of the next; the season start week we define here is the
  ## mid-point of the "off-season"
  flu_data$season <- ifelse(
    flu_data$week <= 30,
    paste0(flu_data$year - 1, "/", flu_data$year),
    paste0(flu_data$year, "/", flu_data$year + 1)
  )
  
  ## Season week column: week number within season
  ## weeks after week 30 get season_week = week - 30
  ## weeks before week 30 get season_week = week + (number of weeks in previous year) - 30
  ## This computation relies on the start_date function in package MMWRweek,
  ## which is not exported from that package's namespace!!!
  flu_data$season_week <- ifelse(
    flu_data$week <= 30,
    flu_data$week + MMWRweek(MMWRweek:::start_date(flu_data$year) - 1)$MMWRweek - 30,
    flu_data$week - 30
  )
  
  flu_data <- as.data.frame(flu_data)
  
  return(flu_data)
}



#' Download and preprocess the latest CDC flu data, state-level
#'
#' @param latest_year year through which data should be downloaded, defaults to current year
#'
#' @return data frame with latest state-level flu data, preprocessed
#' @export
download_and_preprocess_state_flu_data <- function(latest_year = as.numeric(format(Sys.Date(), "%Y"))) {
  
  require(cdcfluview)
  require(MMWRweek)
  require(dplyr)
  require(lubridate)
  
  flu_data_raw <- ilinet(region="state", years=1997:latest_year)
  
  flu_data <- mutate(flu_data_raw, time = as.POSIXct(MMWRweek2Date(year, week)))
  
  ## set rows with denominator zeroes to NAs
  flu_data[which(flu_data$total_patients==0),"weighted_ili"] <- NA
  flu_data[which(flu_data$total_patients==0),"unweighted_ili"] <- NA
  
  ## Add time_index column: the number of days since some origin date
  ## (1970-1-1 in this case).  The origin is arbitrary.
  flu_data$time_index <- as.integer(lubridate::date(flu_data$time) -  ymd("1970-01-01"))
  
  ## Season column: for example, weeks of 2010 up through and including week 30
  ## get season 2009/2010; weeks after week 30 get season 2010/2011
  ## Official CDC flu season for the purposes of prediction runs from week 40 of
  ## one year to week 20 of the next; the season start week we define here is the
  ## mid-point of the "off-season"
  flu_data$season <- ifelse(
    flu_data$week <= 30,
    paste0(flu_data$year - 1, "/", flu_data$year),
    paste0(flu_data$year, "/", flu_data$year + 1)
  )
  
  ## Season week column: week number within season
  ## weeks after week 30 get season_week = week - 30
  ## weeks before week 30 get season_week = week + (number of weeks in previous year) - 30
  ## This computation relies on the start_date function in package MMWRweek,
  ## which is not exported from that package's namespace!!!
  flu_data$season_week <- ifelse(
    flu_data$week <= 30,
    flu_data$week + MMWRweek(MMWRweek:::start_date(flu_data$year) - 1)$MMWRweek - 30,
    flu_data$week - 30
  )
  
  state_flu <- as.data.frame(flu_data)
  
  return(state_flu)
}



#' Download and preprocess the latest CDC hospitalization data
#'
#' @param latest_year year through which data should be downloaded, defaults to current year
#'
#' @return data frame with latest hospitalization data, preprocessed
#' @export
download_and_preprocess_hosp_data <- function(latest_year = as.numeric(format(Sys.Date(), "%Y"))) {
  
  require(cdcfluview)
  require(MMWRweek)
  require(dplyr)
  require(lubridate)
  
  flu_data_raw_hosp <-cdcfluview::hospitalizations(years=2009:latest_year)
  flu_data_raw_hosp$week <- flu_data_raw_hosp$year_wk_num
  flu_data <- mutate(flu_data_raw_hosp, time = as.POSIXct(MMWRweek2Date(year, week)))
  
  ## set rows with denominator zeroes to NAs
  #flu_data[which(flu_data$total_patients==0),"weighted_ili"] <- NA
  
  ## Add time_index column: the number of days since some origin date
  ## (1970-1-1 in this case).  The origin is arbitrary.
  flu_data$time_index <- as.integer(lubridate::date(flu_data$time) -  ymd("1970-01-01"))
  
  ## Season column: for example, weeks of 2010 up through and including week 30
  ## get season 2009/2010; weeks after week 30 get season 2010/2011
  ## Official CDC flu season for the purposes of prediction runs from week 40 of
  ## one year to week 20 of the next; the season start week we define here is the
  ## mid-point of the "off-season"
  flu_data$season <- ifelse(
    flu_data$week <= 30,
    paste0(flu_data$year - 1, "/", flu_data$year),
    paste0(flu_data$year, "/", flu_data$year + 1)
  )
  
  ## Season week column: week number within season
  ## weeks after week 30 get season_week = week - 30
  ## weeks before week 30 get season_week = week + (number of weeks in previous year) - 30
  ## This computation relies on the start_date function in package MMWRweek,
  ## which is not exported from that package's namespace!!!
  flu_data$season_week <- ifelse(
    flu_data$week <= 30,
    flu_data$week + MMWRweek(MMWRweek:::start_date(flu_data$year) - 1)$MMWRweek - 30,
    flu_data$week - 30
  )
  
  hosp_flu <- as.data.frame(flu_data)
  
  return(hosp_flu)
}

