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
  location <- paste0("http://datasets.wikimedia.org/aggregate-datasets/", path,
                     "?ts=", gsub(x = Sys.time(), pattern = "(-| )", replacement = ""))
  con <- url(location)
  return(readr::read_delim(con, delim = "\t", ...))
}