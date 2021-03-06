% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_prefixes}
\alias{get_prefixes}
\title{Wikimedia Language Prefixes}
\format{A data frame with 280-some rows and 3 variables:
\describe{
\item{language}{Name of language in English}
\item{prefix}{Prefix used -- e.g. 'en' for 'English'}
}}
\source{
\url{https://meta.wikimedia.org/wiki/Template:Table_of_Wikimedia_projects#Projects_per_language_codes} # nolint
}
\usage{
get_prefixes()
}
\description{
Returns a dataset containing almost 300 language-prefix pairs
used by multilingual projects (Wikipedia, Wiktionary, Wikibooks, Wikinews,
Wikiquote, Wikisource, Wikiversity, and Wikivoyage).
}
\details{
English names of languages come from SIL --
e.g. \url{http://www-01.sil.org/iso639-3/documentation.asp?id=ady} and are
added to Meta as a Template:Eln --
e.g. \url{https://meta.wikimedia.org/wiki/Template:Eln_ady}
}
\seealso{
\link{update_prefixes}, \link{get_projects}, \link{get_langproj}
}
