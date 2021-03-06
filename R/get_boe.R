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
#' get_boe

get_boe <- function(series, from = as.Date("1975-01-01"), to = Sys.Date()){

  if(class(from) != "Date" | class(to) != "Date")
    stop("to and from must be dates", call. = FALSE)

  url <- paste0("http://www.bankofengland.co.uk/boeapps/iadb/fromshowcolumns.asp?csv.x=yes",
                "&Datefrom=", format.Date(from, "%d/%b/%Y"),
                "&Dateto=", format.Date(to, "%d/%b/%Y"),
                "&SeriesCodes=", paste(toupper(series), collapse = ","),
                "&CSVF=CN&UsingCodes=Y&VPD=Y&VFD=N")

  raw <- suppressWarnings(suppressMessages(readr::read_csv(url)))

  if(ncol(raw) <= 1)
    stop("unknown series", call. = FALSE)

  output <- raw %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = as.Date(date, "%d %b %Y"),
                  value = as.numeric(value))

  output

}
