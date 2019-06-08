#' Fetch BoE timeseries data
#'
#' This function fetches exchange rate data from the Bank of England (BoE) database, see \url{https://www.bankofengland.co.uk/boeapps/database/}
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#' @param date date value, when the forwards are from, default is today's date
#' @param three_point logical value, whether or not forwards should be presented as three-point estimate (minimum, most likely and maximum value)
#' @examples
#' \dontrun{
#' get_usd_forward(three_point = FALSE))
#' get_usd_forward(date = as.Date("2005-01-01"))
#' }
#' @export
#' get_usd_forward


get_usd_forward <- function(date = Sys.Date(), three_point = TRUE){

  forward_lookup <- tibble::tibble(code = c("1", "3", "6", "Y"),
                           forward_length = c(1, 3, 6, 12))

  forward_series <- toupper(paste0("xudlds", c("1", "3", "6", "y")))

  get_forwards <- function(x){

    df <- get_boe(x) %>%
      dplyr::arrange(desc(date)) %>%
      utils::head(30) %>%
      dplyr::mutate(date = max(date),
                    code = stringr::str_sub(series, -1, -1)) %>%
      dplyr::left_join(forward_lookup, by = "code") %>%
      dplyr::group_by(date, series, forward_length) %>%
      dplyr::summarise(value = mean(value)) %>%
      dplyr::ungroup()

    df

  }

  raw_forwards <- purrr::map_dfr(forward_series, get_forwards)

  interpolated_forwards <- raw_forwards %>%
    dplyr::full_join(tibble::tibble(
      forward_length = min(raw_forwards$forward_length):max(raw_forwards$forward_length)),
      by = "forward_length") %>%
    dplyr::arrange(forward_length) %>%
    dplyr::mutate(date = zoo::na.locf(date),
                  forward_month = stringr::str_sub(
                    date %m+% months(forward_length), 1, 7),
                  value = zoo::na.approx(value)) %>%
    dplyr::select(date, forward_month, value)

}
