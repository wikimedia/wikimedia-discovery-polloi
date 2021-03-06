% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maths.R
\name{compress}
\alias{compress}
\title{Convert Numeric Values to use SI suffixes}
\usage{
compress(x, round_by = 2)
}
\arguments{
\item{x}{a vector of numeric or integer values}

\item{round_by}{how many digits to round the resulting numbers by}
}
\description{
takes a numeric vector (e.g. 1200, 1300000) and converts it to
use SI suffixes (e.g. 1.2K, 1.3M)
}
\details{
This function is made available under CC-BY-SA 3.0
}
\references{
\url{https://stackoverflow.com/questions/28159936/formatting-large-currency-or-dollar-values-to-millions-billions/} # nolint
}
\author{
Original code: \href{https://stackoverflow.com/users/1855677/42}{42-};
improvement: Mikhail
}
