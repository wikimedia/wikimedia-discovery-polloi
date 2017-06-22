#' @title Safely retrieve the last N values from an object
#' @description Using [utils::tail] to get the last sequential values in an
#'   object relies on that object being ordered, which it sometimes isn't due
#'   to backfilling. `safe_tail` retrieves the last N values in a "safe" way,
#'   taking the possibility of unordered data into account.
#' @param x an object to tail
#' @param n the number of values to take
#' @param silent whether to produce warnings and messages or not. `TRUE` by
#'   default.
#' @export
safe_tail <- function(x, n, silent = TRUE) {
  if (!is.vector(x) && !is.data.frame(x)) {
    stop("safe_trail() only works with vectors and data frames.")
  }
  # \code{silent} suppresses messages which may be used for debugging
  if (is.vector(x)) {
    return(utils::tail(sort(x), n))
  }
  # Intelligently figure out which column is the date/timestamp column (in case it's not the first column):
  timestamp_column <- names(x)[sapply(x, class) %in% c("Date", "POSIXt", "POSIXlt", "POSIXct")]
  if (length(timestamp_column) == 0) {
    if (!silent) {
      message("No date/timestamp column detected for this dataset. It'd be faster to use tail().")
    }
    return(utils::tail(x, n))
  }
  if (length(timestamp_column) > 1) warning("More than one date/timestamp column detected. Defaulting to the first one.")
  if (!silent) {
    message("Sorting by the date/timestamp column before returning the bottom ", n, " rows.")
  }
  return(utils::tail(x[order(x[[timestamp_column[1]]]), ], n))
}

#' @title Sample Half an Object
#' @description easily sample the top or bottom half of an object.
#' @param x the object to sample from
#' @param top whether it should be the top (TRUE) or bottom (FALSE) half. Set
#'   to TRUE by default.
#' @export
half <- function(x, top = TRUE){
  if (top) {
    return(utils::head(x, n = length(x)/2))
  }
  return(utils::tail(x, n = length(x)/2))
}

#' @title Subset a data frame by a date range
#' @param x Data frame
#' @param range A vector of length 2
#' @param from,to A character or Date object to use as upper/lower bound for
#'   the subsetting.
#' @param date_col Just in case the date column is named something else other
#'   than 'date'
#' @importFrom lubridate ymd
#' @export
subset_by_date_range <- function(x, range = NULL, from = NULL, to = NULL, date_col = "date") {
  if (!is.null(range)) {
    from <- range[1]
    to <- range[2]
  }
  if (is.null(from)) {
    warning('Need a "from" date. Returning the original data frame.')
    return(x)
  }
  if (is.null(to)) {
    warning('No "to" data so defaulting to yesterday.')
    to <- Sys.Date() - 1
  }
  if (is.character(from)) {
    from <- as.Date(from)
  }
  if (is.character(to)) {
    to <- as.Date(to)
  }
  return(x[x[[date_col]] >= from & x[[date_col]] <= to, ])
}

#' @title Safely Combine R Objects of Variying Lengths by Columns
#' @description Take a sequence of vector, matrix or data-frame arguments and
#'   combine by columns or rows, respectively.
#' @param ... Vectors or matrices.
#' @return A matrix with NAs wherever needed.
#' @references \url{http://r.789695.n4.nabble.com/How-to-join-matrices-of-different-row-length-from-a-list-td3177212.html}
#' @export
cbind_fill <- function(...) {
  nm <- lapply(list(...), as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x) rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

#' @title Conditionally Select A Dataset
#' @description Like `ifelse`, but not awful and usable with
#'  `data.frame`s, similar to [dplyr::if_else]
#' @param test an object which can be coerced to logical mode.
#' @param yes_set the dataset to return if `test` is true.
#' @param no_set the dataset to return if `test` is false.
#' @return The appropriate object.
#' @export
data_select <- function(test, yes_set, no_set){
  if (test) {
    return(yes_set)
  }
  return(no_set)
}
