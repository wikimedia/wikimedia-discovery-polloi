context("maths")

test_that("% change", {
  expect_equal(percent_change(1, 2), 100)
  expect_equal(percent_change(1.25, 1.5), 20)
  expect_equal(percent_change(c(1.25, 1.5)), c(NA, 20))
})

test_that("suffixes", {
  expect_equal(compress(c(0, 1, 10, 100)), c("0", "1", "10", "100"))
  expect_equal(compress(1.642e3, round_by = 1), "1.6K")
  expect_equal(compress(c(100000, 1e6, 1e12, 1e9)), c("100K", "1M", "1T", "1B"))
  expect_equal(compress(c(0, 1, 1e6, 1e3)), c("0", "1", "1M", "1K"))
})
