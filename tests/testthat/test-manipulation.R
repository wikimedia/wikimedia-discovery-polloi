context("manipulation")

data("wdqs_usage", package = "polloi")

test_that("Data can be subsetted correctly", {
  expect_equal(nrow(safe_tail(wdqs_usage, 30)), 30)
  expect_equal(safe_tail(wdqs_usage, 1)$date, max(wdqs_usage$date))
  expect_equal(nrow(subset_by_date_range(wdqs_usage, from = "2017-01-01", to = "2017-01-31")), 31)
})

test_that("Objects of different lengths can be c-bound correctly", {
  A <- matrix(1:4, 2, 2)
  B <- matrix(1:6, 3, 2)
  C <- matrix(2:1, 1, 2)
  result <- cbind_fill(A, B, C)
  expect_equal(sum(is.na(result)), 6)
  expect_equal(result[3, , drop = TRUE], c(NA, NA, 3, 6, NA, NA)) # Exclude Linting
  expect_equal(result[, 6, drop = TRUE], c(1, NA, NA))
})
