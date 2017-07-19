library(osc)
context("Loading functions")

test_that("echem_read() returns the right columns", {
  file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
  d <- echem_read(file)
  expect_named(d$data, c('potential', 'current', 'direc', 'change', 'sweep', 'cv'))

  file <- system.file('extdata/cv/Electrolysis.txt', package = 'osc')
  d <- echem_read(file)
  expect_named(d$data, c('time', 'charge', 'current'))
})
