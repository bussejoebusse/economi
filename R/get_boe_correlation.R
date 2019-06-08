#' Calculate correlation between currency movements in BoE data
#'
#' This function calculates the correlation between currency movements in BoE data
#' @importFrom magrittr %>%
#' @param series a vector of BoE currency codes
#' @param steps an integer value detailing the number of different correlation intervals to consider
#' @examples
#' \dontrun{
#' get_boe_correlation("xumauss", 12))
#' }
#' @export
#' get_boe_correlation

get_boe_correlation <- function(series, steps){

  currency_series <- get_boe(series)

  add_lags <- function(x){

    df <- currency_series %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(lagged_value = dplyr::lag(value, x),
                    lags = x)

    df

  }

  lagged_currency_series <- purrr::map_dfr(1:steps, add_lags)

  correlation <- lagged_currency_series %>%
    dplyr::filter(!is.na(lagged_value)) %>%
    dplyr::group_by(series, lags) %>%
    dplyr::summarise(correlation = cor(value, lagged_value))

  correlation

}
