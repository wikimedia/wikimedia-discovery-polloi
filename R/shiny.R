#' @title Standardised Input Selector for Smoothing
#' @description Creates a select list for picking smoothing level.
#' @param input_id ID
#' @param label Label
#' @family inputs
#' @export
smooth_select <- function(input_id, label = "Smoothing") {
  select_input <- shiny::selectInput(
    inputId = input_id, label = label, selectize = TRUE, selected = "global",
    choices = c(
      "Use Global Setting" = "global",
      "No Smoothing" = "day",
      "Weekly Median" = "week",
      "Monthly Median" = "month",
      "Splines" = "gam"
    )
  )
  return(select_input)
}

#' @title Standardised Input Selector for Automata Filtering
#' @description Creates a checkbox for including/excluding automata.
#' @param input_id ID
#' @param label Label
#' @family inputs
#' @export
automata_select <- function(input_id, label = "Include automata (e.g. web crawlers)") {
  checkbox_input <- shiny::checkboxInput(input_id, label = label, value = TRUE)
  return(checkbox_input)
}

#' @title Standardized Drop-down Selector for Time Frame
#' @description Creates a select list that can be used for selecting a time
#'   frame (e.g. "Last 90 days").
#' @param input_id ID
#' @param label Label
#' @return A `selectInput`
#' @family inputs
#' @seealso timeframe_daterange "Shiny Dashboarding"
#' @export
timeframe_select <- function(input_id, label = "Time Frame") {
  select_input <- shiny::selectInput(
    inputId = input_id, label = label, selectize = TRUE, selected = "global",
    choices = c(
      "Use Global Setting" = "global",
      "All available data" = "all",
      "Last 7 days" = "week",
      "Last 30 days" = "month",
      "Last 90 days" = "quarter",
      "Custom" = "custom"
    )
  )
  return(select_input)
}

#' @title Standardized Date Range Selector for Time Frame
#' @description Creates a date ranger selector.
#' @param select_input_id The ID you used for the corresponding
#'  [timeframe_select]
#' @param label Label
#' @return A [shiny::conditionalPanel] containing a [shiny::dateRangeInput]
#' @family inputs
#' @export
timeframe_daterange <- function(select_input_id, label = "Custom Date Range") {
  conditional_panel <-  shiny::conditionalPanel(
    paste0("input.", select_input_id, " == 'custom'"),
    shiny::dateRangeInput(
      inputId = paste(select_input_id, "daterange", sep = "_"),
      label = label,
      start = Sys.Date() - 11,
      end = Sys.Date() - 1,
      min = "2015-04-14"
    )
  )
  return(conditional_panel)
}

#' @title Get The Time Range
#' @description This figures out the date range based on the four different
#'   inputs: local & global timeframe selections and local & global date range
#'   selections. It is used for [subset_by_date_range].
#' @param input_local_timeframe The value of the input corresponding to the
#'   local timeframe selector.
#' @param input_local_daterange The value of the input corresponding to the
#'   local date range selector.
#' @param input_global_timeframe The value of the input corresponding to the
#'   global timeframe selector.
#' @param input_global_daterange The value of the input corresponding to the
#'   global date range selector.
#' @return A Date vector of length 2
#' @examples \dontrun{
#' time_frame_range(
#'   input$timeframe, input$daterange,
#'   input$timeframe_global, input$daterange_global
#' )
#' }
#' @family Shiny Dashboarding
#' @export
time_frame_range <- function(input_local_timeframe,
                             input_local_daterange,
                             input_global_timeframe,
                             input_global_daterange) {
  tf_setting <- input_local_timeframe
  if (tf_setting == "global") {
    if (input_global_timeframe == "custom") {
      return(input_global_daterange)
    } else {
      tf_setting <- input_global_timeframe
    }
  }
  return(switch(
    tf_setting,
    all = c(as.Date("2015-04-14"), Sys.Date() - 1),
    week = c(Sys.Date() - 8, Sys.Date() - 1),
    month = c(Sys.Date() - 31, Sys.Date() - 1),
    quarter = c(Sys.Date() - 91, Sys.Date() - 1),
    custom = input_local_daterange
  ))
}

#' @title Bad Data Value Box
#' @description This is used in cases where something has gone wrong with the
#'   data and a value could not be calculated. This ensures the "bad data" value
#'   boxes have a consistent look.
#' @param subtitle Subtitle text.
#' @family Shiny Dashboarding
#' @importFrom shinydashboard valueBox
#' @export
na_box <- function(subtitle) {
  value_box <- valueBox(subtitle = subtitle, value = "NA", color = "red", icon = icon("warning"))
  return(value_box)
}
