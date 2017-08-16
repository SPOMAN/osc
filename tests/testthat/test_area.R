library(osc)
context("Area functions")

test_that("area() returns the all data", {
  file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
  d <- echem_read(file)
  d_new <- area(d, sw = 1, x1 = -1.85, x2 = -1.40)

  expect_true(!is.null(attr(d_new, "area")))
  expect_true(!is.null(d_new))
  expect_equal(d, d_new)
})

test_that("area() truncates area data to span fitted region", {
  file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
  d <- echem_read(file)
  d <- area(d, sw = 1, x1 = -1.85, x2 = -1.40, span = 0.05)

  expect_gt(length(d$potential), length(attr(d, "area")$data$potential))
  expect_lte(-1.85 - 0.05/2, min(attr(d, "area")$data$potential))
  expect_gte(-1.40 + 0.05/2, max(attr(d, "area")$data$potential))
})
