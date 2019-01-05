#' Fetch ONS timeseries data
#'
#' This function fetches data from the Office for National Statistics (ONS) Time Series Explorer, see \url{https://www.ons.gov.uk/timeseriestool}
#' @importFrom magrittr %>%
#' @param series a vector of ONS timeseries codes
#' @param dataset the name of the ONS dataset which the timeseries codes belong to
#' @examples
#' \dontrun{
#' get_ons("KA46", "EMP")
#' get_ons(c("KA46", "KA4O", "KA4R"), "EMP")
#' }
#' @export
#' get_ons

get_ons <- function(series, dataset){

  get_data <- function(series){

    id <- tolower(series)
    dataset <- tolower(dataset)

    url <- paste0("https://api.ons.gov.uk/dataset/",
                  dataset,
                  "/timeseries/" ,
                  series,
                  "/data")

    request <- httr::GET(url)

    if(request$status_code == 404)
      stop("unknown series or dataset", call. = FALSE)



    raw <- httr::content(request, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()

    df <- raw$months  %>%
      dplyr::mutate(series = series,
                    dataset = dataset,
                    date = lubridate::ymd(paste(date, "01", sep = " ")),
                    value = as.numeric(value)) %>%
      dplyr::select(date, interval, series, dataset, value)

    df
  }

  output <- purrr::map_dfr(series, get_data)

  output

}
