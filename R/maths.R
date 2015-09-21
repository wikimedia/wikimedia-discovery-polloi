#'@title Median Absolute Deviation
#'@description computes the Median Absolute Deviation, or MAD. It's like standard deviations
#'only it doesn't suck terribly for any dataset we'd actually encounter outside a classroom.
#'
#'@param x a numeric vector
#'
#'@export
mad <- function(x){
  median(abs(x - median(x)))
}

#'@title Calculate a Percentage Change
#'
#'@description calculates a delta between X or Y (or sequential X values)
#'expressed as a percentage.
#'
#'@param x a numeric vector
#'
#'@param y (optionally) an additional numeric vector. If y is not provided, each
#'value in x will be compared to the subsequent value - if y is, each x value will
#'be compared to the equivalent y value.
#'
#'@export
percent_change <- function(x, y = NULL) {
  if(is.null(y)) {
    return(100 * (x - c(NA, x[-length(x)])) / c(NA, x[-length(x)]))
  }
  return(100 * (y - x) / x)
}

#'@title Convert Numeric Values to use SI suffixes
#'
#'@description takes a numeric vector (1200, 1300, 1400) and converts it to
#'use SI suffixes (1.2K, 1.3K, 1.4k)
#'
#'@param x a vector of numeric or integer values
#'
#'@param round_by how many digits to round the resulting numbers by.
#'
#'@export
compress <- function(x, round_by = 2) {
  # by StackOverflow user 'BondedDust' : http://stackoverflow.com/a/28160474
  div <- findInterval(as.numeric(gsub("\\,", "", x)),
                      c(1, 1e3, 1e6, 1e9, 1e12) )
  paste(round( as.numeric(gsub("\\,","",x))/10^(3*(div-1)), round_by),
        c("","K","M","B","T")[div], sep = "" )
}