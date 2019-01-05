#' A function to access ons timeseries data
#'
#' This data uses the ons api to access ons time series data
#' @importFrom magrittr %>%
#' @export
#' get_ons

get_ons <- function(id, dataset){
  
  
  get_data <- function(id){
    
    id <- tolower(id)
    dataset <- tolower(dataset)
    
    url <- paste0("https://api.ons.gov.uk/dataset/", 
                  dataset, 
                  "/timeseries/" , 
                  id, 
                  "/data")
    
    request <- httr::GET(url)
    
    if(request$status_code == 404)
      stop(paste("Request Error: check ID and Dataset"))
    
    
    
    raw <- httr::content(request, "text", encoding = "UTF-8") %>% 
      jsonlite::fromJSON()
    
    df <- raw$months  %>% 
      dplyr::mutate(interval = "months",
                    id = id,
                    dataset = dataset,
                    date = lubridate::ymd(paste(date, "01", sep = " ")),
                    value = as.numeric(value)) %>% 
      dplyr::select(date, interval, id, dataset, value)
    
    df
  }
  
  output <- purrr::map_dfr(id, get_data)
  
  output
  
}
