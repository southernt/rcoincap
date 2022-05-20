
#' Helper function for CoinCap API OHLC interval granularities
#'
#' @return named list of granularities with useful parameters
#'
#' @export
coincap_granularities <- function() {
  list(
    five_min = list(
      interval = "m5",
      multiplier = (24 * 12),
      sql_tx = "5 minutes",
      iter_days = 2
    ),
    fifteen_min = list(
      interval = "m15",
      multiplier = (24 * 4),
      sql_tx = "15 minutes",
      iter_days = 7
    ),
    one_hr = list(
      interval = "h1",
      multiplier = 24,
      sql_tx = "1 hour",
      iter_days = 30
    ),
    four_hr = list(
      interval = "h4",
      multiplier = 6,
      sql_tx = "4 hours",
      iter_days = 122
    ),
    one_day = list(
      interval = "d1",
      multiplier = 1,
      sql_tx = "1 day",
      iter_days = 730
    ),
    one_wk = list(
      interval = "w1",
      multiplier = (1 / 7),
      sql_tx = "1 week",
      iter_days = 1461
    )
  )
}

#' Helper function for CoinCap API granularity parameters
#'
#' @param granularity character spelling out (number)_(granularity abbreviation)
#' @param type parameter to return
#'
#' @return character/numeric depending on parameter
#'
#' @export
coincap_granularity <- function(granularity = "one_day", type = "interval") {
  gran_choices <- c(
    "five_min","fifteen_min","one_hr","four_hr","one_day","one_wk"
  )
  granularity <- match.arg(granularity, gran_choices, several.ok = F)
  type = match.arg(
    arg = type,
    choices = c("interval","multiplier","sql_tx","iter_days"),
    several.ok = FALSE
  )
  coincap_granularities()[[granularity]][[type]]
}

#' Map between coin symbols and names
#'
#' @param x character symbol/name
#'
#' @return character symbol/name
#'
#' @name symbol_nm_map
NULL

#' @rdname symbol_nm_map
#'
#' @export
coincap_symbol_to_nm <- function(x) {
  stopifnot(is.character(x))
  arg_ls <- list(base_symbol = tolower(x), limit = 1)
  dat <- do.call(coincap_markets, arg_ls)[,c("base_symbol","base_id")]
  if (inherits(dat, "data.frame") && nrow(dat) > 0) {
    return(stats::setNames(dat$base_id, dat$base_symbol))
  } else {
    stop("no match found")
  }
}
# coincap_symbol_to_nm(x = "btc")

#' @rdname symbol_nm_map
#'
#' @export
coincap_nm_to_symbol <- function(x) {
  stopifnot(is.character(x))
  arg_ls <- list(base_id = tolower(x), limit = 1)
  dat <- do.call(coincap_markets, arg_ls)[,c("base_symbol","base_id")]
  if (inherits(dat, "data.frame") && nrow(dat) > 0) {
    return(stats::setNames(dat$base_symbol, dat$base_id))
  } else {
    stop("no match found")
  }
}
# coincap_nm_to_symbol(x = "bitcoin")

#' Helper function for CoinCap API granularity interval max values
#'
#' @param granularity character value returned by `coincap_granularity()` using `type = "interval"`
#'
#' @return character/numeric depending on parameter
coincap_ohlc_iter_adj <- function(granularity) {
  round(
    (
      coincap_granularity(granularity, "iter_days") * 24 * 60 * 60 * 1000
    ) - 900
  )
}


#' Helper function for CoinCap API URL with query
#'
#' @param endpoint character
#' @param q_ls named list of query key/name value pairs
#'
#' @return character API URL
coincap_query_helper <- function(endpoint, q_ls) {
  require(purrr)
  require(dplyr)
  q_ls <- lapply(q_ls, function(x) {
    if (inherits(x, "name")) return(eval(x))
    x
  })
  q_chr <- paste(
    purrr::imap_chr(q_ls, ~ paste(.y, .x, sep = "=")),
    collapse = "&"
  )
  paste(coincap_api_url(endpoint), q_chr, sep = "?")
}

#' Helper function for CoinCap API OHLC data
#'
#' @param q_ls named list of query key/name value pairs
#'
#' @return data frame
coincap_ohlc_helper <- function(q_ls) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  res <- httr::GET(coincap_query_helper("candles", q_ls))
  if (res$status_code != 200) stop(jsonlite::fromJSON(rawToChar(res$content))$error)
  res_stat <- as.integer(res$status_code)
  if (dplyr::between(res_stat, 400, 417)) {
    stop(paste(
      "api call failed because of a user error",
      jsonlite::fromJSON(rawToChar(res$content))$error,
      sep = " - "
    ))
  } else if (dplyr::between(res_stat, 500, 505)) {
    stop("api call failed because of a server error, maybe try later?")
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content))
  dat$data
}

#' Helper function for processing CoinCap API OHLC data
#'
#' @param df data frame returned by `coincap_ohlc_helper()`
#'
#' @return data frame
clean_coincap_candles <- function(df) {
  require(dplyr)
  require(lubridate)
  dat <- df |>
    dplyr::mutate_if(is.double, millisec_to_datetime) |>
    dplyr::mutate_if(is.character, as.double) |>
    dplyr::select(tm = period, open, high, low, close, volume) |>
    dplyr::arrange(tm) |>
    dplyr::as_tibble()
  dat[-nrow(dat),]
}


#' Query for available assets from the CoinCap API
#'
#' @return data frame
#'
#' @export
coincap_assets <- function() {
  require(httr)
  require(jsonlite)
  require(janitor)
  require(dplyr)
  res <- httr::GET(coincap_api_url("assets"))
  res_stat <- as.integer(res$status_code)
  if (dplyr::between(res_stat, 400, 417)) {
    stop(paste(
      "api call failed because of a user error",
      jsonlite::fromJSON(rawToChar(res$content))$error,
      sep = " - "
    ))
  } else if (dplyr::between(res_stat, 500, 505)) {
    stop("api call failed because of a server error, maybe try later?")
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content))
  dat$data |>
    janitor::clean_names() |>
    dplyr::mutate_at(
      dplyr::vars(rank, supply, max_supply, dplyr::contains("usd"), dplyr::contains("24hr")),
      as.double
    ) |>
    dplyr::mutate(symbol = tolower(symbol)) |>
    dplyr::as_tibble()
}


#' Query for available markets from the CoinCap API
#'
#' @param exchange_id search by exchange id
#' @param base_symbol returns all containing the base symbol
#' @param quote_symbol returns all containing the quote symbol
#' @param base_id returns all containing the base id
#' @param quote_id returns all containing the quote id
#' @param asset_symbol returns all assets containing symbol (base and quote)
#' @param asset_id returns all assets containing id (base and quote)
#' @param limit max limit of 2000
#' @param offset offset
#'
#' @return data frame
#'
#' @export
coincap_markets <- function(
  exchange_id,
  base_symbol,
  quote_symbol,
  base_id,
  quote_id,
  asset_symbol,
  asset_id,
  limit = 2000,
  offset = 1
) {
  require(httr)
  require(jsonlite)
  require(janitor)
  require(dplyr)
  library(lubridate)

  stopifnot(limit <= 2000)
  arg_ls <- as.list(match.call())[-1]
  if (missing(limit)) arg_ls$limit <- limit
  if (missing(offset)) arg_ls$offset <- offset
  names(arg_ls) <- dplyr::case_when(
    endsWith(names(arg_ls), "_id") ~ gsub("_id", "Id", names(arg_ls)),
    endsWith(names(arg_ls), "_symbol") ~ gsub("_symbol", "Symbol", names(arg_ls)),
    TRUE ~ names(arg_ls)
  )

  res <- httr::GET(coincap_query_helper("markets", arg_ls))
  res_stat <- as.integer(res$status_code)
  if (dplyr::between(res_stat, 400, 417)) {
    stop(paste(
      "api call failed because of a user error",
      jsonlite::fromJSON(rawToChar(res$content))$error,
      sep = " - "
    ))
  } else if (dplyr::between(res_stat, 500, 505)) {
    stop("api call failed because of a server error, maybe try later?")
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content))
  dat$data |>
    janitor::clean_names() |>
    dplyr::mutate_at(
      vars(rank, percent_exchange_volume, dplyr::starts_with("price"), dplyr::contains("24hr")),
      as.double
    ) |>
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("symbol")), tolower) |>
    dplyr::mutate(updated = lubridate::as_datetime(updated / 1000)) |>
    dplyr::as_tibble()
}
# coincap_markets()


#' Query for available markets from the CoinCap API
#'
#' @param exchange exchange id
#' @param interval candle interval
#' @param base_id base id
#' @param quote_id quote id
#' @param start UNIX time in milliseconds, omitting will return the most recent candles (ex: `1528410925604`)
#' @param end UNIX time in milliseconds, omitting will return the most recent candles (ex: `1528411045604`)
#'
#' @return data frame
#'
#' @export
coincap_ohlc <- function(
  base_id = "bitcoin",
  quote_id = "tether",
  granularity = "one_day",
  exchange = "binance",
  start_tm = Sys.time() - (coincap_ohlc_iter_adj(granularity) / 1000L),
  end_tm = Sys.time()
) {
  require(httr)
  require(jsonlite)
  require(janitor)
  require(dplyr)

  arg_ls <- as.list(match.call())[-1]
  if (missing(base_id)) arg_ls$baseId <- base_id
  if (missing(quote_id)) arg_ls$quoteId <- quote_id
  if (missing(granularity)) arg_ls$granularity <- granularity
  if (missing(exchange)) arg_ls$exchange <- exchange
  if (missing(start_tm)) arg_ls$start_tm <- start_tm
  if (missing(end_tm)) arg_ls$end_tm <- end_tm
  names(arg_ls) <- dplyr::case_when(
    names(arg_ls) == "granularity" ~ "interval",
    endsWith(names(arg_ls), "_id") ~ gsub("_id", "Id", names(arg_ls)),
    endsWith(names(arg_ls), "_tm") ~ gsub("_tm", "", names(arg_ls)),
    TRUE ~ names(arg_ls)
  )
  arg_ls <- lapply(arg_ls, function(x) {
    if (inherits(x, "name")) return(eval(x))
    x
  })
  arg_ls$start <- datetime_to_millisec(arg_ls$start)
  arg_ls$end <- datetime_to_millisec(arg_ls$end)
  gran_loop <- coincap_ohlc_iter_adj(arg_ls$interval)
  arg_ls$interval <- coincap_granularity(arg_ls$interval)

  if ((arg_ls$end - arg_ls$start) > gran_loop) {
    o_start <- arg_ls$start
    arg_ls$start <- arg_ls$end - gran_loop
    dat_ls <- list()
    dat_ls[[1L]] <- tryCatch(
      { do.call(coincap_ohlc_helper, list(q_ls = arg_ls)) },
      error = function(e) "error"
    )
    if (inherits(dat_ls[[1L]], "data.frame")) {
      continue_ind <- TRUE
    } else {
      stop("initial api request failed")
    }
    k <- 2L
    while (continue_ind && arg_ls$start > o_start) {
      arg_ls$end <- arg_ls$start - 900L
      arg_ls$start <- arg_ls$end - gran_loop
      if (arg_ls$start < o_start) arg_ls$start <- o_start
      dat_tmp <- tryCatch({
        # Sys.sleep(.3)
        do.call(coincap_ohlc_helper, list(q_ls = arg_ls))
      },
      error = function(e) "error"
      )
      if (inherits(dat_tmp, "data.frame")) {
        dat_ls[[k]] <- dat_tmp
        k <- k + 1L
        continue_ind <- ifelse(arg_ls$start > o_start, TRUE, FALSE)
      } else {
        continue_ind <- FALSE
      }
    }
    dat <- dplyr::bind_rows(dat_ls)
  } else {
    dat <- coincap_ohlc_helper(arg_ls)
  }
  clean_coincap_candles(dat)
}


# coincap_ohlc_2 <- function(
#   base_id = "bitcoin",
#   quote_id = "tether",
#   granularity = "fifteen_min",
#   exchange = "binance",
#   start_tm = Sys.time() - (coincap_ohlc_iter_adj(granularity) / 1000L),
#   end_tm = Sys.time()
# ) {
#   require(httr)
#   require(jsonlite)
#   require(janitor)
#   require(dplyr)
#   require(wrapr)
#   require(parallel)
#
#   arg_ls <- as.list(match.call())[-1]
#   if (missing(base_id)) arg_ls$baseId <- base_id
#   if (missing(quote_id)) arg_ls$quoteId <- quote_id
#   if (missing(granularity)) arg_ls$granularity <- granularity
#   if (missing(exchange)) arg_ls$exchange <- exchange
#   if (missing(start_tm)) arg_ls$start_tm <- start_tm
#   if (missing(end_tm)) arg_ls$end_tm <- end_tm
#   names(arg_ls) <- dplyr::case_when(
#     names(arg_ls) == "granularity" ~ "interval",
#     endsWith(names(arg_ls), "_id") ~ gsub("_id", "Id", names(arg_ls)),
#     endsWith(names(arg_ls), "_tm") ~ gsub("_tm", "", names(arg_ls)),
#     TRUE ~ names(arg_ls)
#   )
#   arg_ls <- lapply(arg_ls, function(x) {
#     if (inherits(x, "name")) return(eval(x))
#     x
#   })
#   arg_ls$start <- datetime_to_millisec(arg_ls$start)
#   arg_ls$end <- datetime_to_millisec(arg_ls$end)
#   gran_loop <- coincap_ohlc_iter_adj(arg_ls$interval)
#   arg_ls$interval <- coincap_granularity(arg_ls$interval)
#
#   if ((arg_ls$end - arg_ls$start) > gran_loop) {
#     tm_ls_helper <- function(x) list(start = (x - gran_loop), end = x)
#     tm_ls <- list(tm_ls_helper(arg_ls$end))
#     k <- 1L
#     while (tm_ls[[k]][["start"]] > arg_ls$start) {
#       k <- k + 1L
#       tm_ls[[k]] <- tm_ls_helper(tm_ls[[k - 1L]][["start"]] - 900L)
#     }
#     tm_ls[[k]][["start"]] <- max(c(tm_ls[[k]][["start"]], arg_ls$start))
#     df_tm <- dplyr::bind_rows(tm_ls) |> dplyr::arrange(start)
#
#     tmp_arg_ls <- arg_ls[setdiff(names(arg_ls), c("start","end"))]
#     dat_ls <- list()
#     dat_ls[[1L]] <- tryCatch({
#       do.call(
#         coincap_ohlc_helper,
#         list(q_ls = append(tmp_arg_ls, list(start = df_tm$start[[1L]], end = df_tm$end[[1L]])))
#       )
#     },
#     error = function(e) "error"
#     )
#     if (inherits(dat_ls[[1L]], "data.frame")) {
#       continue_ind <- TRUE
#     } else {
#       stop("initial api request failed")
#     }
#     k <- 2L
#     while (k <= nrow(df_tm) && continue_ind) {
#       dat_tmp <- tryCatch({
#         coincap_ohlc_helper(
#           append(tmp_arg_ls, list(start = df_tm$start[[k]], end = df_tm$end[[k]]))
#         )
#       },
#       error = function(e) "error"
#       )
#       if (inherits(dat_tmp, "data.frame")) {
#         dat_ls[[k]] <- dat_tmp
#       } else {
#         continue_ind <- FALSE
#       }
#       k <- k + 1L
#     }
#     dat <- dplyr::bind_rows(dat_ls)
#   } else {
#     dat <- coincap_ohlc_helper(arg_ls)
#   }
#   clean_coincap_candles(dat)
# }

# cl <- parallel::makeCluster(2)
# parallel::clusterEvalQ(cl, library("httr"))
# parallel::clusterEvalQ(cl, library("jsonlite"))
# parallel::clusterEvalQ(cl, library("dplyr"))
# parallel::clusterExport(cl, c("coincap_api_url","coincap_query_helper","coincap_ohlc_helper"), envir = environment())
# parallel::clusterExport(cl, c("tmp_arg_ls","coincap_ohlc_loop_helper"), envir = environment())
#
# dat <- wrapr::execute_parallel(
#   tables = list(df_tm_ranges = df_tm),
#   f = coincap_ohlc_loop_helper,
#   partition_column = "chunk",
#   cl = cl
#   )
#
# parallel::stopCluster(cl)
