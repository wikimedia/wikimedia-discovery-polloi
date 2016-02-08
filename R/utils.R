#'@title Update internal index of language prefixes.
#'@description Scrapes the table off
#'  \url{https://meta.wikimedia.org/wiki/Table_of_Wikimedia_projects} and stores the first
#'  three columns containing the language, local language, and the prefix code.
#'@importFrom magrittr "%>%" set_names
#'@importFrom xml2 read_html
#'@importFrom rvest html_nodes html_table
#'@importFrom readr write_csv
#'@export
update_prefixes <- function() {
  if (!requireNamespace("rvest", quietly = TRUE) && !requireNamespace("xml2", quietly = TRUE)) {
    stop("Requires rvest and xml2 packages to be installed.")
  }

  file_location <- system.file("extdata/prefixes.csv", package = "polloi")

  prefixes <- read_html("https://meta.wikimedia.org/wiki/Table_of_Wikimedia_projects#Projects_per_language_codes") %>%
    html_nodes(".wikitable") %>%
    { .[[1]] } %>%
    html_table() %>%
    { .[, c('English Name', 'Local Name', 'Language Code')] } %>%
    set_names(c('language', 'local', 'prefix'))
  prefixes$prefix <- sub(":", "", prefixes$prefix)
  prefixes$prefix <- gsub("-", "_", prefixes$prefix)
  prefixes$language <- sub("Template:Eln ", "", prefixes$language)

  readr::write_csv(prefixes, file_location)
  return(invisible())
}

#'@title Update internal index of wiki projects.
#'@description Updates the internal list of Wikimedia projects from the package
#'  repository to avoid having to reinstall the package from source.
#'@import httr
#'@export
update_projects <- function() {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Requires httr package to be installed.")
  }

  file_location <- system.file("extdata/projects.csv", package = "polloi")

  result <- httr::GET("https://raw.githubusercontent.com/wikimedia/wikimedia-discovery-polloi/master/inst/extdata/projects.csv")
  httr::stop_for_status(result)
  result <- httr::content(result)

  cat(result, file = file_location)
  return(invisible())
}

#'@title Parse Wikiid into Language-Project
#'@description Uses the internal index of languages, prefixes, wikis, and
#'  projects to parse a wikiid into a language-project tuple.
#'@param x A character vector of wikiid's.
#'@return A data frame with columns 'language' and 'project'.
#'@importFrom dplyr left_join
#'@export
parse_wikiid <- function(x) {
  temp <- get_langproj()
  result <- left_join(data.frame(wikiid = x, stringsAsFactors = FALSE), temp, by = "wikiid")
  return(result[, c('language', 'project')])
}
