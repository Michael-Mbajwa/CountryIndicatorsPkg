# Is the function returning an expected value for a given input?
test_that("rank indicators by country works", {
  returned_value <- rank_indicators_by_country(indicator_name = "Adults (ages 15+) and children (ages 0-14) newly infected with HIV")

  expected_value <- readr::read_csv("test_data_rank_indicators_by_country.csv")%>%mutate_at(c("Year"), as.character)

  expect_equal(returned_value, expected_value)
})


# Can the function deal with all kinds of input?
test_that("wrong indicator_name provided", {
  expect_error({rank_indicators_by_country(indicator_name = "hiv")}, "not contained")
})


test_that("no parameter provided", {
  expect_error({rank_indicators_by_country()}, "must be")
})


test_that("wrong year type provided", {
  expect_error({rank_indicators_by_country(indicator_name="Adults (ages 15+) and children (ages 0-14) newly infected with HIV"
                                           , year=2021)}
               , "must be")})


test_that("wrong n type provided", {
  expect_error({rank_indicators_by_country("Current health expenditure per capita (current US$)", year="2002", n="10")}, "must be")
})


test_that("wrong n range provided", {
  expect_error({rank_indicators_by_country("Current health expenditure per capita (current US$)", year="2002", n=300)}, "must have")
})


test_that("year does not exist", {
  expect_error({rank_indicators_by_country(indicator_name="Adults (ages 15+) and children (ages 0-14) newly infected with HIV"
                                           , year="1700")}
               , "does not exist")})
