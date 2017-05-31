#' @title Check 'n' Notify
#' @description Check a dataset for missing data from the day before and the
#'  past week, and create a `notificationItem` if missing data has been
#'  detected.
#' @param dataset A data.frame with a 'date' column.
#' @param label The label to use in the notification.
#' @family Shiny Dashboarding
#' @name check_notify
NULL

#' @rdname check_notify
#' @importFrom shinydashboard notificationItem
#' @importFrom shiny icon
#' @export
check_yesterday <- function(dataset, label) {
  # e.g. label = "desktop events"
  yesterday_date <- Sys.Date() - 1
  if (!(yesterday_date %in% dataset$date)) {
    return(notificationItem(text = paste("No", label," from yesterday."),
                            icon = icon("table"), status = "danger"))
  }
  return(NULL)
}

#' @rdname check_notify
#' @importFrom shinydashboard notificationItem
#' @importFrom shiny icon
#' @export
check_past_week <- function(dataset, label) {
  past_week <- Sys.Date() - c(2:7) # Sys.Date()-1 is already handled by check_yesterday()
  if (any(!(past_week %in% dataset$date))) {
    return(notificationItem(text = paste("No", label," from past week."),
                            icon = icon("table"), status = "danger"))
  }
  return(NULL)
}
