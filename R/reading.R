#'@title Read Datasets from datasets.wikimedia
#'@description Grabs datasets from the public directories at
#'\href{https://datasets.wikimedia.org}{datasets.wikimedia.org} for use in dashboarding.
#'
#'@param path the path to the file, starting from datasets.wikimedia.org/aggregate-datasets/
#'@param ... Additional arguments to be passed to \code{readr::read_delim}
#'
#'@importFrom readr read_delim
#'@export
read_dataset <- function(path, ...){
  location <- paste0("https://datasets.wikimedia.org/aggregate-datasets/", path,
                     "?ts=", gsub(x = Sys.time(), pattern = "(-| )", replacement = ""))
  con <- url(location)
  data <- read_delim(con, delim = "\t", ...)
  # De-duplicate & return:
  dupes <- duplicated(data)
  if (any(dupes)) {
    dupe_dates <- paste(unique(as.character(data[dupes, ][[1]], format = "%Y-%m-%d")), collapse = ", ")
    warning("Duplicated data detected in '", path, "' for the following dates: ", dupe_dates)
  }
  return(data[!dupes, ])
}