#'@title Standardised Input Selector for Smoothing
#'@param input_id ID
#'@param label Label
#'@family inputs
#'@export
smooth_select <- function(input_id, label = "Smoothing") {
  return(selectInput(inputId = input_id, label = label, selectize = TRUE,
                     selected = "global", choices = c("Use Global Setting" = "global",
                                                      "No Smoothing" = "day", "Weekly Median" = "week", "Monthly Median" = "month")))
}
#'@title Standardized Drop-down Selector for Time Frame
#'@param input_id ID
#'@param label Label
#'@return A \code{selectInput}
#'@family inputs
#'@seealso timeframe_daterange "Shiny Dashboarding"
#'@export
timeframe_select <- function(input_id, label = "Time Frame") {
  return(selectInput(inputId = input_id, label = label, selectize = TRUE, selected = "global",
                     choices = c("Use Global Setting" = "global", "All available data" = "all",
                                 "Last 7 days" = "week", "Last 30 days" = "month",
                                 "Last 90 days" = "quarter", "Custom" = "custom")))
}
#'@title Standardized Date Range Selector for Time Frame
#'@param select_input_id The ID you used for the corresponding
#'  \code{timeframe_select}
#'@param label Label
#'@return A \code{conditionalPanel} containing a \code{dateRangeInput}
#'@family inputs
#'@seealso timeframe_select
#'@export
timeframe_daterange <- function(select_input_id, label = "Custom Date Range") {
  return(conditionalPanel(paste0("input.", select_input_id," == 'custom'"),
                          dateRangeInput(paste(select_input_id, "daterange", sep = "_"), label = label,
                                         start = Sys.Date()-11, end = Sys.Date()-1, min = "2015-04-14")))
}

#'@title Get The Time Range
#'@description This figures out the date range based on the four different
#'inputs: local & global timeframe selections and local & global date range
#'selections. It is used for \code{subset_by_date_range}.
#'@param input_local_timeframe The value of the input corresponding to the
#'  local timeframe selector.
#'@param input_local_daterange The value of the input corresponding to the
#'  local date range selector.
#'@param input_global_timeframe The value of the input corresponding to the
#'  global timeframe selector.
#'@param input_global_daterange The value of the input corresponding to the
#'  global date range selector.
#'@return A Date vector of length 2
#'@examples
#'\dontrun{
#'time_frame_range(input$timeframe, input$daterange,
#'                 input$timeframe_global, input$daterange_global)
#'}
#'@seealso \code{\link{subset_by_date_range}}
#'@family Shiny Dashboarding
#'@export
time_frame_range <- function(input_local_timeframe,
                             input_local_daterange,
                             input_global_timeframe,
                             input_global_daterange) {
  tf_setting <- input_local_timeframe
  if ( tf_setting == 'global' ) {
    if ( input_global_timeframe == 'custom' ) {
      return(input_global_daterange)
    } else {
      tf_setting <- input_global_timeframe
    }
  }
  return(switch(tf_setting,
                all = c(as.Date("2015-04-14"), Sys.Date()-1),
                week = c(Sys.Date()-8, Sys.Date()-1),
                month = c(Sys.Date()-31, Sys.Date()-1),
                quarter = c(Sys.Date()-91, Sys.Date()-1),
                custom = input_local_daterange))
}
