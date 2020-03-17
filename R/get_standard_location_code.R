cdc_to_delphi_location_dict <- jsonlite::fromJSON(
'{
  "national": "nat",
  "us national": "nat",
  "region1": "hhs1",
  "region2": "hhs2",
  "region3": "hhs3",
  "region4": "hhs4",
  "region5": "hhs5",
  "region6": "hhs6",
  "region7": "hhs7",
  "region8": "hhs8",
  "region9": "hhs9",
  "region10": "hhs10",
  "region 1": "hhs1",
  "region 2": "hhs2",
  "region 3": "hhs3",
  "region 4": "hhs4",
  "region 5": "hhs5",
  "region 6": "hhs6",
  "region 7": "hhs7",
  "region 8": "hhs8",
  "region 9": "hhs9",
  "region 10": "hhs10",
  "hhs region 1": "hhs1",
  "hhs region 2": "hhs2",
  "hhs region 3": "hhs3",
  "hhs region 4": "hhs4",
  "hhs region 5": "hhs5",
  "hhs region 6": "hhs6",
  "hhs region 7": "hhs7",
  "hhs region 8": "hhs8",
  "hhs region 9": "hhs9",
  "hhs region 10": "hhs10",
  "new england": "cen1",
  "mid-atlantic": "cen2",
  "east north central": "cen3",
  "west north central": "cen4",
  "south atlantic": "cen5",
  "east south central": "cen6",
  "west south central": "cen7",
  "mountain": "cen8",
  "pacific": "cen9",
  "alabama": "al",
  "alaska": "ak",
  "arizona": "az",
  "arkansas": "ar",
  "california": "ca",
  "colorado": "co",
  "connecticut": "ct",
  "delaware": "de",
  "florida": "fl",
  "georgia": "ga",
  "hawaii": "hi",
  "idaho": "id",
  "illinois": "il",
  "indiana": "in",
  "iowa": "ia",
  "kansas": "ks",
  "kentucky": "ky",
  "louisiana": "la",
  "maine": "me",
  "maryland": "md",
  "massachusetts": "ma",
  "michigan": "mi",
  "minnesota": "mn",
  "mississippi": "ms",
  "missouri": "mo",
  "montana": "mt",
  "nebraska": "ne",
  "nevada": "nv",
  "new hampshire": "nh",
  "new jersey": "nj",
  "new mexico": "nm",
  "new york": "ny_minus_jfk",
  "north carolina": "nc",
  "north dakota": "nd",
  "ohio": "oh",
  "oklahoma": "ok",
  "oregon": "or",
  "pennsylvania": "pa",
  "rhode island": "ri",
  "south carolina": "sc",
  "south dakota": "sd",
  "tennessee": "tn",
  "texas": "tx",
  "utah": "ut",
  "vermont": "vt",
  "virginia": "va",
  "washington": "wa",
  "west virginia": "wv",
  "wisconsin": "wi",
  "wyoming": "wy",
  "american samoa": "as",
  "commonwealth of the northern mariana islands": "mp",
  "district of columbia": "dc",
  "guam": "gu",
  "puerto rico": "pr",
  "virgin islands": "vi",
  "chicago": "ord",
  "los angeles": "lax",
  "new york city": "jfk"
}'
)

#' Convert a location name to standardized name as given here:
#' https://github.com/cmu-delphi/delphi-epidata/blob/master/src/acquisition/fluview/fluview_locations.py
#'
#' @param location: a character vector of non-standardized location names
#'
#' @return a character vector of standardized location names
#'
#' @export
get_standard_location_code <- function(location) {
  return(
    purrr::map_chr(
      location,
      function(one_loc) {
        if(any(cdc_to_delphi_location_dict == one_loc)) {
          return(one_loc)
        } else {
          result <- cdc_to_delphi_location_dict[[tolower(one_loc)]]
          if(is.null(result)) {
            stop(paste0("No standard location code for \"", one_loc, "\"."))
          }
          return(result)
        }
      }
    ) %>%
      unname()
  )
}
