library(osctools)
context("Loading functions")

test_that("echem_read() returns the expected columns", {
  # The echem_read() function should be able to read datafiles from
  # different potentiostats with different settings. These tests evaluate
  # that a set of example_files return the expected columns.
  files <- c(
    system.file('extdata/cv/cv_example.txt', package = 'osctools'),
    system.file('extdata/cv/cv_example2.txt', package = 'osctools'),
    system.file('extdata/cv/cv_example3.txt', package = 'osctools')
  )

  purrr::map(files, ~ expect_named(echem_read(.x), c('potential', 'current', 'direc', 'change', 'sweep', 'cv')))

  d <- echem_read(system.file('extdata/cv/cv_example4.txt', package = 'osctools'), type = 'CV')
  expect_named(d, c('potential', 'current', 'direc', 'change', 'sweep', 'cv'))

  file <- system.file('extdata/electrolysis/electrolysis_example.txt', package = 'osctools')
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
