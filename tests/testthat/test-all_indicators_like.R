library(readr)
# Is the function returning an expected value for a given input?

test_that("selecting indicators like hiv works", {
  returned_value <- all_indicators_like("hiv")

  expected_value <- readr::read_csv("test_data_all_indicators_like.csv")|>dplyr::collect()|>as.matrix()|>as.vector()

  expect_equal(returned_value, expected_value)
})


# Can the function deal with all kinds of input?
test_that("wrong string provided", {

  expect_warning({all_indicators_like(123)}, "No indicator like")
})


test_that("no parameter provided", {
  expect_error({all_indicators_like()}, "must be")
})
