library(readr)
# Is the function returning an expected value for a given input?
test_that("country key details works", {
  returned_value <- country_key_details(country_name = "nigeria")

  expected_value <- readr::read_csv("test_data_country_key_details_nigeria.csv")

  expect_equal(returned_value, expected_value)
})


test_that("country leader works", {
  returned_value <- country_key_details(country_code = "usa")

  expected_value <- readr::read_csv("test_data_country_key_details_usa.csv")

  expect_equal(returned_value, expected_value)
})


# Can the function deal with all kinds of input?
test_that("no parameter provided", {
  expect_error({country_key_details()}, "were provided")
})


test_that("wrong country name provided", {
  expect_error({country_key_details(country_name = "usa")}, "not contained in")
})


test_that("wrong country code provided", {
  expect_error({country_key_details(country_code = "nigeria")}, "not contained in")
})


test_that("wrong country name type provided", {
  expect_error({country_key_details(country_name = 12345)}, "must be")
})


test_that("wrong country code type provided", {
  expect_error({country_key_details(country_code = 12345)}, "must be")
})
