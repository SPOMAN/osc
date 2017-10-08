library(osctools)
context("Metadata parser")

test_that("extract_metadata() correctly parses numbers", {
  header <- c("Number_one = 0.1", "Number_two : 5.00e-002", "Number_three: -2e2", "Number_four = +0.3", "Number_five : 5", "Not_a_number = P")

  expect_equal(extract_metadata(header, "Number_one"), 0.1)
  expect_equal(extract_metadata(header, "Number_two"), 0.05)
  expect_equal(extract_metadata(header, "Number_three"), -200)
  expect_equal(extract_metadata(header, "Number_four"), 0.3)
  expect_equal(extract_metadata(header, "Number_five"), 5)

  expect_equal(extract_metadata(header, "Not_a_number", pattern = "(P|N)$", numeric = FALSE), "P")
})
