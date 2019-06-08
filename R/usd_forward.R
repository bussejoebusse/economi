#' Fetch BoE timeseries data
#'
#' This function fetches exchange rate data from the Bank of England (BoE) database, see \url{https://www.bankofengland.co.uk/boeapps/database/}
#' @importFrom magrittr %>%
#' @param series a vector of BoE currency codes
#' @param from start date, default is 1 January 1975 (when BoE records begin)
#' @param to end date, default is today's date
#' @examples
#' \dontrun{
#' get_boe(c("xumauss", "xumaers"))
#' get_boe("xumauss", from = as.Date("2000-01-01"), to = as.Date("2005-01-01"))
#' }
#' @export
#' forward_forecast


usd_forward <- function(interval = "month", date = Sys.Date(), three_point = TRUE){

  interval_lookup <- tibble::tibble(type = c("month", "quarter"),
                                    code = c("ma", "qa"))

  selected_interval <- interval_lookup %>%
    dplyr::filter(type == tolower(interval)) %>%
    dplyr::pull(code)

  if(vector.is.empty(selected_interval))
    stop("unknown interval", call. = FALSE)

  series <- paste0("xu", )

}
