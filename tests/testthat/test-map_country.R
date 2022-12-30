test_that("wrong country", {
  expect_error({map_country(all_country_details=country_key_details("nigeira"))}, "not contained")
})
