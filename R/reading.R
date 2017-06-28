#' @title Read Datasets from datasets.wikimedia
#' @description Grabs datasets from the public directories at
#'  [WMF Analytics' Datasets](https://analytics.wikimedia.org/datasets/) for
#'  use in dashboarding.
#' @param path the path to the file, starting from https://analytics.wikimedia.org/datasets/
#' @param force_uncached If `TRUE`, appends a `?ts=...` to URL to force an uncached version
#' @param silent if `TRUE`, suppresses a warning about duplicated data being found
#' @param ... Additional arguments to be passed to [readr::read_delim]
#' @export
read_dataset <- function(path, force_uncached = TRUE, silent = FALSE, ...) {
  location <- paste0("https://analytics.wikimedia.org/datasets/", path)
  if (force_uncached) {
    location <- paste0(location, "?ts=", gsub(x = Sys.time(), pattern = "(-| )", replacement = ""))
  }
  con <- url(location)
  data <- readr::read_delim(con, delim = "\t", ...)
  # De-duplicate & return:
  dupes <- duplicated(data)
  if (any(dupes) && !silent) {
    dupe_dates <- paste(unique(as.character(data[dupes, ][[1]], format = "%Y-%m-%d")), collapse = ", ")
    warning("Duplicated data detected in '", path, "' for the following dates: ", dupe_dates)
  }
  return(data[!dupes, ])
}
