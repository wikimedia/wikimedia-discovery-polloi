% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manipulate.R
\name{safe_tail}
\alias{safe_tail}
\title{Safely retrieve the last N values from an object}
\usage{
safe_tail(x, n, silent = TRUE)
}
\arguments{
\item{x}{an object to tail}

\item{n}{the number of values to take}

\item{silent}{whether to produce warnings and messages or not. \code{TRUE} by
default.}
}
\description{
Using \link[utils:tail]{utils::tail} to get the last sequential values in an
object relies on that object being ordered, which it sometimes isn't due
to backfilling. \code{safe_tail} retrieves the last N values in a "safe" way,
taking the possibility of unordered data into account.
}
