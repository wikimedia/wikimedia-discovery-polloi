#' @title Wikimedia Language Prefixes
#' @description Returns a dataset containing almost 300 language-prefix pairs
#'   used by multilingual projects (Wikipedia, Wiktionary, Wikibooks, Wikinews,
#'   Wikiquote, Wikisource, Wikiversity, and Wikivoyage).
#' @format A data frame with 280-some rows and 3 variables:
#' \describe{
#'   \item{language}{Name of language in English}
#'   \item{prefix}{Prefix used -- e.g. 'en' for 'English'}
#' }
#' @details English names of languages come from SIL --
#'   e.g. \url{http://www-01.sil.org/iso639-3/documentation.asp?id=ady} and are
#'   added to Meta as a Template:Eln --
#'   e.g. \url{https://meta.wikimedia.org/wiki/Template:Eln_ady}
#' @source \url{https://meta.wikimedia.org/wiki/Template:Table_of_Wikimedia_projects#Projects_per_language_codes} # nolint
#' @seealso [update_prefixes], [get_projects], [get_langproj]
#' @export
get_prefixes <- function() {
  return(readr::read_csv(system.file("extdata/prefixes.csv", package = "polloi"), col_types = "cc"))
}

#' @title Wikimedia Projects
#' @description Returns a dataset containing almost 40 Wikimedia Foundation
#'   projects.
#' @format A data frame with 280-some rows and 3 variables:
#' \describe{
#'   \item{wikiid}{The wiki ID used internally.}
#'   \item{project}{Name of the Wikimedia project.}
#'   \item{multilingual}{A boolean indicator whether the project has language
#'     subdomains.}
#' }
#' @source \url{https://meta.wikimedia.org/wiki/Table_of_Wikimedia_projects}
#' @seealso [update_projects], [get_prefixes], [get_langproj]
#' @export
get_projects <- function() {
  projects <- readr::read_csv(
    system.file("extdata/projects.csv", package = "polloi"),
    col_types = "cclc"
  )
  return(projects)
}

#' @title Wikimedia Language-Project Tuples
#' @description Returns a dataset containing all the projects and all possible
#'   language-project combinations for multilingual projects.
#' @details We use a Cartesian product here because we cannot trust the matrix
#'   of languages and projects to be up-to-date, because it is not usually
#'   updated when a new wiki is deployed. The product method ensures that all
#'   the multilingual wikis are correctly captured (provided we have the
#'   latest prefixes) by [parse_wikiid].
#' @importFrom magrittr "%>%"
#' @importFrom tidyr separate
#' @seealso [get_prefixes], [get_projects]
#' @export
get_langproj <- function() {
  prefixes <- get_prefixes()
  projects <- get_projects()
  temp <- data.frame(
    wikiid = as.vector(
      outer(prefixes$prefix,
            projects$wikiid[projects$multilingual],
            paste0
      )
    ),
    name = as.vector(
      outer(
        prefixes$language,
        projects$project[projects$multilingual],
        paste, sep = ":"
      )
    ),
    stringsAsFactors = FALSE)
  projects <- projects[!projects$multilingual, c("wikiid", "language", "project")]
  result <- temp %>%
    separate("name", into = c("language", "project"), sep = ":") %>%
    rbind(projects, .)
  return(result)
}

#' @title Countries and Regions with Traffic to Wikipedia.org
#' @description Names of countries and names of regions in United States,
#'   for example: "U.S. (South)"
#' @format `portal_regions` is a character vector containing about 230
#'   country/region names with traffic to Wikipedia.org (portal).
#' @source \url{https://analytics.wikimedia.org/datasets/discovery/metrics/portal/all_country_data.tsv}
#' @seealso [update_portal_regions]
"portal_regions"

#' @title Wikidata Query Service usage dataset
#' @description WDQS usage dataset is used for unit testing and examples.
#' @format A `data.frame` with over 600 rows and 4 columns:
#' \describe{
#'   \item{date}{`Date`}
#'   \item{homepage}{Web requests made to query.wikidata.org/}
#'   \item{LDF endpoint}{Requests made to query.wikidata.org/bigdata/ldf}
#'   \item{SPARQL endpoint}{Requests made to [WDQS SPARQL endpoint](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service)} # nolint
#' }
#' @source \url{https://analytics.wikimedia.org/datasets/discovery/metrics/wdqs/basic_usage.tsv}
#' @seealso [get_sample_data]
"wdqs_usage"

#' @title U.S. States and Regions
#' @description Returns a dataset containing all U.S. states' and territories'
#'   names, abbreviations and regions.
#' @format A data frame with 56 rows and 3 variables:
#' \describe{
#'   \item{abb}{The abbreviations of U.S. states and territories.}
#'   \item{region}{The regions of U.S. states and territories. See
#'    https://phabricator.wikimedia.org/T136257#2399411.}
#'   \item{state}{The names of U.S. states and territories.}
#' }
#' @source `ISO_3166_1` from package `ISOcodes`; `state.name`, `state.abb` and
#'  `state.region` from package `datasets`; see
#'   \url{https://phabricator.wikimedia.org/T136257#2399411} for U.S. regions.
#' @export
get_us_state <- function() {
  us_states <- readr::read_csv(
    system.file("extdata/us_state_region.csv", package = "polloi"),
    col_types = "ccc"
  )
  return(us_states)
}

#' @title All Countries and U.S. States
#' @description Returns a dataset containing all countries' and U.S. states'
#'   names and abbreviations.
#' @format A data frame with 300 rows and 2 variables:
#' \describe{
#'   \item{abb}{The abbreviations of all countries and U.S. states.}
#'   \item{name}{The names of all countries and U.S. states.}
#' }
#' @source `ISO_3166_1` from package `ISOcodes`; `state.name`, `state.abb` and
#'  `state.region` from package `datasets`.
#' @export
get_country_state <- function() {
  country_states <- readr::read_csv(
    system.file("extdata/all_countries_us_states.csv", package = "polloi"),
    col_types = "cc"
  )
  return(country_states)
}
