% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_sample_data}
\alias{get_sample_data}
\title{Get the latest Wikidata Query Service usage data}
\usage{
get_sample_data(dev = FALSE)
}
\arguments{
\item{dev}{Logical flag that controls whether fetched data is saved to the
\strong{data/} directory (\code{dev = TRUE}) or to where \emph{polloi} is installed
(default)}
}
\description{
Fetches and saves WDQS usage data (successful requests from
known non-automata users), which can be used for unit tests and examples.
}
