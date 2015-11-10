#'@title Construct a Standard Wikimedia Discovery Dygraph
#'@description Construct a dygraph using the custom formatting Wikimedia dashboards use
#'as standard. This is nothing special - a standard dygraph with a bit of custom CSS
#'shipped with the package - but it's surrounded by code that allows the function to
#'turn all our usual data.frame formats into XTS objects.
#'
#'@param data a data.frame reformatted to be XTS-able.
#'
#'@param xlab the label to use for the dygraph's x-axis.
#'
#'@param ylab the label to use for the dygraph's y-axis.
#'
#'@param title the title to use on the dygraph.
#'
#'@param legend_name a custom name for the variable in the event that \code{is_single} is TRUE.
#'NULL by default (in which case the variable will be named "events").
#'
#'@param use_si whether to use si labelling (1000 becomes 1K). TRUE by default.
#'
#'@param expr an optional expression to evaluate prior to building the dygraph.
#'  We use this in (for example) reactive graphing.
#'
#'@param group Group to associate this plot with. The x-axis zoom level of
#'  plots within a group is automatically synchronized.
#'
#'@param ... Additional parameters to pass on to \code{dyOptions}.
#'
#'@importFrom dygraphs renderDygraph dyCSS dyOptions dyLegend dygraph
#'
#'@importFrom xts xts
#'
#'@importFrom RColorBrewer brewer.pal
#'
#'@importFrom magrittr "%>%"
#'
#'@export
make_dygraph <- function(data, xlab, ylab, title,
                         legend_name = NULL, use_si = TRUE, expr = NULL,
                         group = NULL, ...) {

  #Evaluate the expression
  expr

  # Make sure we're not dealing with tbl_df or data.table nonsense
  class(data) <- "data.frame"

  # If we've only got a single variable reformatting into an XTS looks weird, but otherwise we're
  # all cool.
  if (ncol(data) == 2) {
    if (is.null(legend_name)) {
      legend_name <- names(data)[2]
    }
    data <- xts(data[[2]], data[[1]])
    names(data) <- legend_name
  } else {
    data <- xts::xts(data[,-1], order.by = data[[1]])
  }

  # Construct and return the dygraph
  return(dygraph(data, main = title, xlab = xlab, ylab = ylab, group = group) %>%
           dyLegend(width = 400, show = "always") %>%
           dyOptions(strokeWidth = 3,
                     colors = brewer.pal(max(3, ncol(data)), "Set2"),
                     drawPoints = FALSE, pointSize = 3, labelsKMB = use_si,
                     includeZero = TRUE, ...) %>%
           dyCSS(css = system.file("custom.css", package = "polloi")))
}

#'@title Select a Colour Conditionally
#'@description select a colour based on the true/false nature of a condition.
#'Uses green as the "true" colour by default, "red" as false, and
#'
#'@param condition a condition to be evaluated to produce a single TRUE or FALSE value
#'
#'@param true_color the colour used to represent a TRUE result. Green by default.
#'
#'@export
cond_color <- function(condition, true_color = "green") {
  if (is.na(condition)) {
    return("black")
  }

  colours <- c("green","red")
  return(ifelse(condition, true_color, colours[!colours == true_color]))
}

#'@title Select an appropriate directional icon
#'@description allows you to select an appropriate directional icon for a change
#'in condition.
#'
#'@param condition a condition to be evaluated to produce a single TRUE/FALSE
#'
#'@param true_direction which direction represents a positive change. "up" by
#'default.
#'
#'@importFrom shiny icon
#'@export
cond_icon <- function(condition, true_direction = "up") {

  if (true_direction == "up") {
    return(shiny::icon(ifelse(condition, "arrow-up", "arrow-down")))
  }

  return(shiny::icon(ifelse(condition, "arrow-down", "arrow-up")))
}

#'@title Custom Axis Formatter
#'@description This is a JS date formatter to be used in \code{dyAxis} when
#'  it's desired to have the dates on the axis look like "Monday (8/13)", in
#'  other words: "Day of the week (MM/DD)"
#'@export
#'@examples
#'\dontrun{
#'polloi::make_dygraph(data) %>%
#'  dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter)
#'}
custom_axis_formatter <- 'function (d, gran) {
var weekday = new Array(7);
weekday[0]=  "Sunday";
weekday[1] = "Monday";
weekday[2] = "Tuesday";
weekday[3] = "Wednesday";
weekday[4] = "Thursday";
weekday[5] = "Friday";
weekday[6] = "Saturday";
return weekday[d.getDay()] + " (" + (d.getMonth()+1) + "/" + d.getDate() + ")";
}'