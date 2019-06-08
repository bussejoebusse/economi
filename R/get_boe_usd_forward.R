#' Fetch BoE US Dollar forward exchange rate data
#'
#' This function fetches US Dollar forward exchange rate data from the Bank of England (BoE) database, see \url{https://www.bankofengland.co.uk/boeapps/database/}, and uses this data to forecast future US Dollar exchange rates
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
#' get_boe_usd_forward


get_boe_usd_forward <- function(date = Sys.Date(), three_point = TRUE){

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
    dplyr::select(date, forward_month, forward_length, value)

  if(three_point == FALSE){

    interpolated_forwards

  }else{

    error_bounds_raw <- get_boe_error_bounds("xumauss")

    error_bounds <- error_bounds_raw %>%
      dplyr::bind_rows(tibble::tibble(
        steps = dplyr::pull(dplyr::distinct(error_bounds_raw, steps), steps),
        type = rep("Most Likely", max(error_bounds_raw$steps)),
        perc_change = rep(0, max(error_bounds_raw$steps)))) %>%
      dplyr::rename(forward_length = steps)

    three_point_estimate <- interpolated_forwards %>%
      dplyr::left_join(error_bounds, by = "forward_length") %>%
      dplyr::mutate(value = value * ((100 + perc_change) / 100)) %>%
      dplyr::select(date, forward_month, forward_length, type, value) %>%
      dplyr::arrange(desc(type), forward_length)

    three_point_estimate

  }

}
