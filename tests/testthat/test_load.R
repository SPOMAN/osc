library(osc)
context("Loading functions")

test_that("echem_read() returns the right columns", {
  file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
  d <- echem_read(file)
  expect_named(d, c('potential', 'current', 'direc', 'change', 'sweep', 'cv'))

  file <- system.file('extdata/cv/Electrolysis.txt', package = 'osc')
  d <- echem_read(file)
  expect_named(d, c('time', 'charge', 'current'))
})

test_that("meta() returns a list of metadata", {
  data <- tibble::tibble(x = 1, y = 2)
  metadata <- list(
    param1 = 1.0,
    param2 = 2.0
  )
  attr(data, "meta") <- metadata

  testthat::expect_equal(meta(data), metadata)
})
