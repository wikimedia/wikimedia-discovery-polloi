% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_country_state}
\alias{get_country_state}
\title{All Countries and U.S. States}
\format{A data frame with 300 rows and 2 variables:
\describe{
\item{abb}{The abbreviations of all countries and U.S. states.}
\item{name}{The names of all countries and U.S. states.}
}}
\source{
\code{ISO_3166_1} from package \code{ISOcodes}; \code{state.name}, \code{state.abb} and
\code{state.region} from package \code{datasets}.
}
\usage{
get_country_state()
}
\description{
Returns a dataset containing all countries' and U.S. states'
names and abbreviations.
}
