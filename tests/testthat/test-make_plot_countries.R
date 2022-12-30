test_that("wrong data type provided", {
  expect_error({make_plot_countries(map_title=123)}, "must be")
})
