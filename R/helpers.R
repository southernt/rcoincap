#' Helper function for base CoinCap API URL
#'
#' @param endpoint character
#'
#' @return character API URL
coincap_api_url <- function(endpoint) {
  if (missing(endpoint)) return("api.coincap.io/v2")
  # paste("api.coincap.io/v2", endpoint, sep = "/")
  file.path("api.coincap.io/v2", endpoint)
}


#' Helper functions for converting between millisecond numerics and datetime objects
#'
#' @name datetime_millisec
#'
#' @param x numeric/datetime
#'
#' @return numeric/datetime
NULL

#' @rdname datetime_millisec
#'
#' @export
millisec_to_datetime <- function(x) {
  require(lubridate)
  stopifnot(is.numeric(x))
  lubridate::as_datetime(x / 1000)
}

#' @rdname datetime_millisec
#'
#' @export
datetime_to_millisec <- function(x) {
  as.numeric(x) * 1000
}
