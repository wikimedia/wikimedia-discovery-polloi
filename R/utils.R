#' @title Update internal index of language prefixes
#' @description Fetches & processes site matrix to get a list of language
#'   codes and names.
#' @param dev Logical flag that controls whether fetched prefix list is saved
#'   to the **data/** directory (`dev = TRUE`) or to where *polloi* is
#'   installed (default)
#' @importFrom magrittr "%>%"
#' @export
update_prefixes <- function(dev = FALSE) {
  meta_json <- jsonlite::read_json("https://meta.wikimedia.org/w/api.php?action=sitematrix&format=json")
  meta_json$sitematrix$count <- NULL
  meta_json$sitematrix$specials <- NULL
  language_codes <- unname(purrr::map_chr(meta_json$sitematrix, ~ .x$code))
  language_names <- purrr::map(meta_json$sitematrix, ~ .x$localname) %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII")
  language_codes[language_codes == "be-x-old"] <- "be-tarask"

  file_location <- system.file("extdata/prefixes.csv", package = "polloi")
  if (file_location == "") {
    file_location <- "inst/extdata/prefixes.csv"
  }
  dplyr::tibble(language = language_names, prefix = language_codes) %>%
    readr::write_csv(file_location)

  if (dev) {
    file.copy(from = file_location, to = "inst/extdata/prefixes.csv", overwrite = TRUE)
  }
  return(invisible(NULL))
}

#' @title Update internal index of active wikis
#' @description Fetches & processes site matrix to get a list of active wikis.
#' @param dev Logical flag that controls whether fetched prefix list is saved
#'   to the **data/** directory (`dev = TRUE`) or to where *polloi* is
#'   installed (default)
#' @importFrom magrittr "%>%"
#' @export
update_active_wikis <- function(dev) {
  meta_json <- jsonlite::read_json("https://meta.wikimedia.org/w/api.php?action=sitematrix&format=json")
  meta_json$sitematrix$count <- NULL
  meta_json$sitematrix$specials <- NULL
  meta_json$sitematrix$`0`$code

  get_status <- function(sites) {
    return(purrr::map_lgl(sites, ~ "closed" %in% names(.x)))
  }
  get_url <- function(sites) {
    return(purrr::map_chr(sites, ~ .x$url))
  }
  get_dbname <- function(sites) {
    return(purrr::map_chr(sites, ~ .x$dbname))
  }
  get_code <- function(sites) {
    return(purrr::map_chr(sites, ~ .x$code))
  }
  get_df <- function(sites) {
    return(dplyr::tibble(
      dbname = get_dbname(sites),
      project = get_code(sites),
      url = get_url(sites),
      closed = get_status(sites)
    ))
  }

  names(meta_json$sitematrix) <- purrr::map_chr(meta_json$sitematrix, ~ .x$code)

  site_matrix <- purrr::map_dfr(meta_json$sitematrix, ~ get_df(.x$site), .id = "prefix")
  active_wikis <- site_matrix %>%
    dplyr::filter(!closed) %>%
    dplyr::select(-closed)
  active_wikis$prefix[active_wikis$prefix == "be-x-old"] <- "be-tarask"

  file_location <- system.file("extdata/active_wikis.csv", package = "polloi")
  if (file_location == "") {
    file_location <- "inst/extdata/active_wikis.csv"
  }
  readr::write_csv(active_wikis, file_location)

  if (dev) {
    file.copy(from = file_location, to = "inst/extdata/active_wikis.csv", overwrite = TRUE)
  }
  return(invisible(NULL))
}

#' @title Update internal index of wiki projects
#' @description Updates the internal list of Wikimedia projects from the
#'   package repository to avoid having to reinstall the package from source.
#' @import httr
#' @export
update_projects <- function() {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Requires httr package to be installed.")
  }

  file_location <- system.file("extdata/projects.csv", package = "polloi")

  result <- httr::GET(
    "https://raw.githubusercontent.com/wikimedia/wikimedia-discovery-polloi/master/inst/extdata/projects.csv",
    user_agent("'polloi' R package <https://gerrit.wikimedia.org/r/plugins/gitiles/wikimedia/discovery/polloi/>")
  )
  httr::stop_for_status(result)
  result <- httr::content(result)

  cat(result, file = file_location)
  return(invisible(NULL))
}

#' @title Parse Wikiid into Language-Project
#' @description Uses the internal index of languages, prefixes, wikis, and
#'   projects to parse a wikiid into a language-project tuple.
#' @param x A `character` vector of wikiid's
#' @return A `data.frame` with columns 'language' and 'project'
#' @importFrom dplyr left_join
#' @export
parse_wikiid <- function(x) {
  temp <- get_langproj()
  temp$wikiid <- sub("-", "_", temp$wikiid)
  result <- left_join(data.frame(wikiid = x, stringsAsFactors = FALSE), temp, by = "wikiid")
  return(result[, c("language", "project")])
}

#' @title Update Country and Region Names with Traffic to Wikipedia.org
#' @description Get unique country and region names from the **country** column of
#'   \url{https://analytics.wikimedia.org/datasets/discovery/metrics/portal/all_country_data.tsv}.
#' @param dev Logical flag that controls whether fetched country list is saved
#'   to the **data/** directory (`dev = TRUE`) or to where *polloi* is
#'   installed (default)
#' @export
update_portal_regions <- function(dev = FALSE) {
  portal_regions <- read_dataset("discovery/metrics/portal/all_country_data.tsv", col_types = "Dcididid")
  portal_regions <- sort(c(unique(portal_regions$country), "United States"))
  usethis::use_data(portal_regions, pkg = ifelse(dev, ".", system.file(package = "polloi")), overwrite = TRUE)
}

#' @title Get the latest Wikidata Query Service usage data
#' @description Fetches and saves WDQS usage data (successful requests from
#'   known non-automata users), which can be used for unit tests and examples.
#' @param dev Logical flag that controls whether fetched data is saved to the
#'   **data/** directory (`dev = TRUE`) or to where *polloi* is installed
#'   (default)
#' @importFrom magrittr "%>%"
get_sample_data <- function(dev = FALSE) {
  wdqs_usage <- read_dataset("discovery/metrics/wdqs/basic_usage.tsv", col_types = "Dclli") %>%
    dplyr::arrange(date) %>%
    dplyr::filter(http_success, !is_automata) %>%
    dplyr::select(-c(http_success, is_automata)) %>%
    dplyr::mutate(path = dplyr::case_when(
      path == "/" ~ "homepage",
      path == "/bigdata/namespace/wdq/sparql" ~ "SPARQL endpoint",
      path == "/bigdata/ldf" ~ "LDF endpoint",
      TRUE ~ as.character(NA)
    )) %>%
    dplyr::filter(!is.na(path)) %>%
    tidyr::spread(path, events, fill = 0) %>%
    as.data.frame
  usethis::use_data(wdqs_usage, pkg = ifelse(dev, ".", system.file(package = "polloi")), overwrite = TRUE)
}
