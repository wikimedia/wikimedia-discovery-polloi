#' @title Switch between global and local smoothing
#' @description We use a lot of smoothing in our reactive graphs, and tend to
#'  offer both global (entire dashboard) and local (tab-specific) smoothing
#'  options. `smooth_switch` is a simple function to abstract away the
#'  logic behind determining whether the global or local option should be
#'  relied on.
#' @param global the input variable for global smoothing.
#' @param local the input variable for local smoothing.
#' @export
smooth_switch <- function(global, local) {
  if (local == "global") {
    return(global)
  }
  return(local)
}

#' @title Dynamically Smooth Data
#' @description Takes an untidy (read: dygraph-appropriate) dataset and adds
#' columns for each variable consisting of the smoothed, averaged mean.
#' @param dataset an untidy, dygraph-appropriate `data.frame`
#' @param smooth_level the level of smoothing. Options are "day", "week",
#'   "month", and "gam" (for smoothing via splines)
#' @param rename whether to rename the fields once smoothed. `TRUE` by default
#' @examples
#' data(wdqs_usage)
#' smoother(wdqs_usage, "week")
#' smoother(wdqs_usage, "month")
#' smoother(wdqs_usage, "gam")
#' @export
smoother <- function(dataset, smooth_level = "day", rename = TRUE) {
  # Determine the names and levels of aggregation. By default
  # a smoothing level of "day" is assumed, which is no smoothing
  # whatsoever, and so the original dataset is returned.
  switch(
    smooth_level,
    week = {
      dataset$filter_1 <- lubridate::week(dataset[[1]])
      dataset$filter_2 <- lubridate::year(dataset[[1]])
      name_append <- ifelse(rename, " (Weekly average)", "")
    },
    month = {
      dataset$filter_1 <- lubridate::month(dataset[[1]])
      dataset$filter_2 <- lubridate::year(dataset[[1]])
      name_append <- ifelse(rename, " (Monthly average)", "")
    },
    gam = {
      smoothed <- as.data.frame(do.call(
        cbind,
        lapply(dataset[, -1, drop = FALSE], function(var_to_smooth) {
          return(tryCatch({
            fit <- mgcv::gam(
              y ~ s(x, k = 14),
              data = data.frame(
                x = as.numeric(dataset$date),
                y = var_to_smooth,
                stringsAsFactors = FALSE
              )
            )
            unname(mgcv::predict.gam(
              fit,
              newdata = data.frame(x = as.numeric(dataset$date))
            ))
          }, error = function(e) {
            return(NA)
          }))
        })))
      return(cbind(date = dataset$date, smoothed + (1 * is.na(dataset[, -1])) * dataset[, -1]))
    },
    {
      return(dataset)
    }
  )

  # If we're still here it was weekly or monthly. Calculate
  # the average for each unique permutation of filters
  result <- plyr::ddply(
    .data = dataset,
    .variables = c("filter_1", "filter_2"),
    .fun = function(df, name_append) {

      # Construct output names for the averages, compute those averages, and
      # apply said names.
      output_names <- paste0(names(df)[2:(ncol(df) - 2)], name_append)
      holding <- df[, 2:(ncol(df) - 2), drop = FALSE] %>%
        apply(2, FUN = stats::median) %>%
        round %>%
        t %>%
        as.data.frame
      names(holding) <- output_names

      # Return the bound original values and averaged values
      return(cbind(df[, 1, drop = FALSE], holding))
    },
    name_append = name_append
  )
  return(result[, !(names(result) %in% c("filter_1", "filter_2"))])
}
