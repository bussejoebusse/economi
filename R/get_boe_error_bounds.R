#' Calculate error bounds for BoE currency data
#'
#' This function calculates error bounds for BoE currency data. Error bounds are based off of percentage changes in a given BoE series over different intervals.
#' @importFrom magrittr %>%
#' @param series a vector of BoE currency codes
#' @param steps an integer of how many steps to calculate error bounds over
#' @param percentile a numerical value detailing the percentile at which currency movements are evaluated
#' @examples
#' \dontrun{
#' get_boe_error_bounds("xumauss"))
#' get_boe_error_bounds("xumauss", steps = 24, percentile = 0.90)
#' }
#' @export
#' get_boe_error_bounds

get_boe_error_bounds <- function(series, steps = 12, percentile = 0.95){

  currency_series <- get_boe(series)

  add_lags <- function(x){

    df <- currency_series %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(lagged_value = dplyr::lag(value, x),
                    lags = x)

    df

  }

  lagged_currency_series <- purrr::map_dfr(1:steps, add_lags)

  currency_perc_change <- lagged_currency_series %>%
    dplyr::mutate(perc_change = ((value / lagged_value) - 1) * 100,
                  type = dplyr::case_when(perc_change < 0 ~ "Minimum",
                                          perc_change > 0 ~ "Maximum")) %>%
    dplyr::filter(!is.na(lagged_value),
                  !is.na(type))

  currency_percentile_perc_change <- currency_perc_change %>%
    dplyr::group_by(series, lags, type) %>%
    dplyr::summarise(perc_change = quantile(abs(perc_change), percentile, na.rm = T))

  strictly_increasing <- function(y){

    max <- currency_percentile_perc_change %>%
      dplyr::filter(lags <= y) %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(max = max(perc_change))

    df <- currency_percentile_perc_change %>%
      dplyr::filter(lags == y) %>%
      dplyr::left_join(max, by = c("type")) %>%
      dplyr::mutate(perc_change = ifelse(perc_change < max, max, perc_change)) %>%
      dplyr::select(-max)

    df

  }

  currency_percentile_perc_change_increasing <- purrr::map_dfr(1:steps, strictly_increasing)

  output <- currency_percentile_perc_change_increasing %>%
    dplyr::mutate(perc_change = ifelse(type == "Minimum", perc_change * -1, perc_change),
                  confidence_int = percentile) %>%
    dplyr::rename(steps = lags) %>%
    dplyr::arrange(type, steps)

  output

}

