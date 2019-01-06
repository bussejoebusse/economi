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

    url <- paste0("https://api.ons.gov.uk/dataset/",
                  tolower(dataset),
                  "/timeseries/" ,
                  tolower(series),
                  "/data")

    request <- httr::GET(url)

    if(request$status_code == 404)
      stop("unknown series or dataset", call. = FALSE)

    raw <- httr::content(httr::GET(url), "text", encoding = "UTF-8")

    parsed <- jsonlite::fromJSON(raw)

    tidy_data <- function(data){

      df <- data  %>%
        dplyr::mutate(series = toupper(series),
                      dataset = toupper(dataset),
                      value = as.numeric(value)) %>%
        dplyr::select(date, series, dataset, value)

    }

    if(length(parsed$months > 0)){

    tidy <- tidy_data(parsed$months)  %>%
      dplyr::mutate(date = lubridate::ymd(paste(date, "01", sep = " ")))

    }else

      if(length(parsed$quarters > 0)){

        tidy <- tidy_data(parsed$quarters) %>%
          dplyr::mutate(date = lubridate::yq(date))

      }else

        tidy <- tidy_data(parsed$years) %>%
          dplyr::mutate(date = lubridate::ymd(paste0(date, "01", "01")))

    tidy
  }

  output <- purrr::map_dfr(series, get_data)

  output

}
