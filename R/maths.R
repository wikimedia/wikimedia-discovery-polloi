#' @title Calculate a Percentage Change
#' @description calculates a delta between X or Y (or sequential X values)
#'   expressed as a percentage.
#' @param x a numeric vector
#' @param y (optionally) an additional numeric vector. If y is not provided,
#'   each value in x will be compared to the subsequent value - if y is, each x
#'   value will be compared to the equivalent y value
#' @export
percent_change <- function(x, y = NULL) {
  if (is.null(y)) {
    return(100 * (x - c(NA, x[-length(x)])) / c(NA, x[-length(x)]))
  }
  return(100 * (y - x) / x)
}

#' @title Convert Numeric Values to use SI suffixes
#' @description takes a numeric vector (e.g. 1200, 1300000) and converts it to
#'   use SI suffixes (e.g. 1.2K, 1.3M)
#' @details This function is made available under CC-BY-SA 3.0
#' @param x a vector of numeric or integer values
#' @param round_by how many digits to round the resulting numbers by
#' @author Original code: [42-](https://stackoverflow.com/users/1855677/42);
#'   improvement: Mikhail
#' @references \url{https://stackoverflow.com/questions/28159936/formatting-large-currency-or-dollar-values-to-millions-billions/}
#' @export
compress <- function(x, round_by = 2) {
  div <- findInterval(x, c(1, 1e3, 1e6, 1e9, 1e12))
  return(paste0(round( x / 10 ^ (3 * (div - 1)), round_by), c("", "", "K", "M", "B", "T")[div + 1]))
}
