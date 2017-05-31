#' @title Wikimedia Language Prefixes
#' @description Returns a dataset containing almost 300 language-prefix pairs
#'   used by multilingual projects (Wikipedia, Wiktionary, Wikibooks, Wikinews,
#'   Wikiquote, Wikisource, Wikiversity, and Wikivoyage).
#'
#' @format A data frame with 280-some rows and 3 variables:
#' \describe{
#'   \item{language}{Name of language in English.}
#'   \item{local}{Name of language in local language.}
#'   \item{prefix}{Prefix used -- e.g. 'en' for 'English'.}
#' }
#'
#' @details English names of languages come from SIL --
#'   e.g. \url{http://www-01.sil.org/iso639-3/documentation.asp?id=ady} and are
#'   added to Meta as a Template:Eln --
#'   e.g. \url{https://meta.wikimedia.org/wiki/Template:Eln_ady}
#'
#' @source \url{https://meta.wikimedia.org/wiki/Template:Table_of_Wikimedia_projects#Projects_per_language_codes}
#' @importFrom readr read_csv
#' @seealso [update_prefixes], [get_projects], [get_langproj]
#' @export
get_prefixes <- function() {
  return(readr::read_csv(system.file("extdata/prefixes.csv", package = "polloi")))
}

#' @title Wikimedia Projects
#' @description Returns a dataset containing almost 40 Wikimedia Foundation
#'   projects.
#'
#' @format A data frame with 280-some rows and 3 variables:
#' \describe{
#'   \item{wikiid}{The wiki ID used internally.}
#'   \item{project}{Name of the Wikimedia project.}
#'   \item{multilingual}{A boolean indicator whether the project has language
#'     subdomains.}
#' }
#'
#' @source \url{https://meta.wikimedia.org/wiki/Table_of_Wikimedia_projects}
#' @importFrom readr read_csv
#' @seealso [update_projects], [get_prefixes], [get_langproj]
#' @export
get_projects <- function() {
  return(readr::read_csv(system.file("extdata/projects.csv", package = "polloi")))
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
  temp <- data.frame(wikiid = as.vector(outer(prefixes$prefix,
                                              projects$wikiid[projects$multilingual],
                                              paste0)),
                     name = as.vector(outer(prefixes$language,
                                            projects$project[projects$multilingual],
                                            paste, sep = ":")),
                     stringsAsFactors = FALSE)
  projects <- projects[!projects$multilingual, c('wikiid', 'language', 'project')]
  result <- temp %>%
    separate("name", into = c("language", "project"), sep = ":") %>%
    rbind(projects, .)
  return(result)
}
