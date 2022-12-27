# Is the function returning an expected value for a given input?
test_that("country leader works", {
  returned_value <- country_leader(country_name = "luxembourg")

  expected_value <- readr::read_csv("test_data_country_leader_luxembourg.csv")

  expect_equal(returned_value, expected_value)
})


test_that("country leader works", {
  returned_value <- country_leader(country_code = "usa")

  expected_value <- readr::read_csv("test_data_country_leader_usa.csv")

  expect_equal(returned_value, expected_value)
})


# Can the function deal with all kinds of input?
test_that("no argument provided", {
  expect_error({country_leader()}, "were provided")
})


test_that("no argument provided", {
  expect_error({country_leader(country_code = "luxembourg")}, "not contained")
})


test_that("no argument provided", {
  expect_error({country_leader(country_name = "usa")}, "not contained")
})
