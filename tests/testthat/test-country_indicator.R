# Is the function returning an expected value for a given input?
test_that("country indicator works", {
  returned_value <- country_indicator(country_code = "usa", indicator_contains = "health", year = "2002")

  expected_value <- readr::read_csv("test_data_country_indicator.csv")
  expect_equal(returned_value$Indicator_Name, expected_value$Indicator_Name)
})


# Can the function deal with all kinds of input?
test_that("indicator_name or year not provided", {
  expect_error({country_indicator(country_code = "usa", indicator_contains = "health")}
               , "must be provided")
})


test_that("indicator_name or year not provided", {
  expect_error({country_indicator(country_code = "usa", year = "2002")}
               , "must be provided")
})


test_that("country_code or country_name must be provided", {
  expect_error({country_indicator(indicator_contains = "health", year = "2002")}
               , "must be provided")
})


test_that("provided year is a string", {
  expect_error({country_indicator(country_code = "usa", indicator_contains = "health", year = 2002)}
               , "strings")
})


test_that("wrong country code provided", {
  expect_error({country_indicator(country_code = "Uk", indicator_contains = "health", year = "2002")}
               , "not contained in")
})


test_that("wrong country name provided", {
  expect_error({country_indicator(country_name = "Nigerian", indicator_contains = "health", year = "2002")}
               , "not contained in")
})


test_that("wrong indicator contains provided", {
  expect_error({country_indicator(country_name = "Nigeria", indicator_contains = "2345", year = "2002")}
               , "not contained in")
})


test_that("wrong year provided", {
  expect_error({country_indicator(country_name = "Nigeria", indicator_contains = "fuel", year = "2022")}
               , "not exist in")
})
