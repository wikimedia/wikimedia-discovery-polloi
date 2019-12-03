context("manipulation")

data("wdqs_usage", package = "polloi")

test_that("Data can be subsetted correctly", {
  expect_equal(nrow(safe_tail(wdqs_usage, 30)), 30)
  expect_equal(safe_tail(wdqs_usage, 1)$date, max(wdqs_usage$date))
  expect_equal(nrow(subset_by_date_range(wdqs_usage, from = "2017-01-01", to = "2017-01-31")), 31)
})

test_that("Objects of different lengths can be c-bound correctly", {
  x <- matrix(1:4, 2, 2)
  y <- matrix(1:6, 3, 2)
  z <- matrix(2:1, 1, 2)
  result <- cbind_fill(x, y, z)
  expect_equal(sum(is.na(result)), 6)
  expect_equal(result[3, , drop = TRUE], c(NA, NA, 3, 6, NA, NA)) # nolint
  expect_equal(result[, 6, drop = TRUE], c(1, NA, NA))
})

test_that("Capitalization works", {
  expect_equal(capitalize_first_letter("hello world"), "Hello World")
  expect_equal(capitalize_first_letter(c("abc is", "easy as 123")), c("Abc Is", "Easy As 123"))
})

test_that("Columns are correctly reordered", {
  df <- data.frame(date = "2017-08-31", A = 1, B = 3, C = 2)
  expect_equal(names(reorder_columns(df)), c("date", "B", "C", "A"))
})
