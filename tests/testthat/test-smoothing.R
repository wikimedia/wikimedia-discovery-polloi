context("smoothing")

data("wdqs_usage", package = "polloi")
subsetted_wdqs_usage <- subset_by_date_range(wdqs_usage, from = "2017-05-01", to = "2017-05-31")
smoothed_wk <- smoother(subsetted_wdqs_usage, smooth_level = "week", rename = TRUE)
smoothed_mo <- smoother(subsetted_wdqs_usage, smooth_level = "month", rename = FALSE)
smoothed_spline <- smoother(subsetted_wdqs_usage, smooth_level = "gam", rename = TRUE)

test_that("Renaming works", {
  expect_equal(
    names(smoothed_wk),
    c("date", "homepage (Weekly average)", "LDF endpoint (Weekly average)", "SPARQL endpoint (Weekly average)")
  )
  expect_equal(
    names(smoothed_mo),
    c("date", "homepage", "LDF endpoint", "SPARQL endpoint")
  )
  expect_equal(
    names(smoothed_spline),
    c("date", "homepage", "LDF endpoint", "SPARQL endpoint")
  )
})

test_that("Smoothing works correctly", {
  expect_equal(smoothed_wk$`homepage (Weekly average)`[c(1, 31)], c(1218, 1281))
  expect_equal(smoothed_mo$`LDF endpoint`, rep(11, 31))
  expect_equal(smoothed_spline$`SPARQL endpoint`[c(1, 31)], c(63935, 95314), tolerance = 0.1)
})
