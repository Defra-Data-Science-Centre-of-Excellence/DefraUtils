test_that("expected output is returned", {
  gdp_series <- get_ons_series(2015:2020, index = "GDP")
  expect_s3_class(gdp_series, "tbl_df")
  expect_equal(sapply(colnames(gdp_series), \(x) class(gdp_series[[x]])),
               c(year = "numeric", year_end = "yearqtr", index = "numeric"))
  expect_equal(last(gdp_series$index), 100)
})

test_that("index option works as expected", {
  expect_false(isTRUE(all.equal(
    get_ons_series(2015:2020, index = "GDP"),
    get_ons_series(2015:2020, index = "CPIH")
  )))
})

test_that("custom URLs work as expected", {
  expect_identical(
    get_ons_series(2015:2020, ons_url = "https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7g7/mm23"),
    get_ons_series(2015:2020, ons_url = "economy/inflationandpriceindices/timeseries/d7g7/mm23"),
    get_ons_series(2015:2020, ons_url = "https://www.ons.gov.uk/generator?format=csv&uri=economy/inflationandpriceindices/timeseries/d7g7/mm23")
  )
})

test_that("no index or URL returns error", {
  expect_error(get_ons_series(2015:2020), "Please provide either index or URL")
})

test_that("creates specified file", {
  save_dir <- tempdir()
  save_file <- file.path(save_dir, "ons_series_l522_mm23.csv")
  on.exit(unlink(save_file))
  expect_false(file.exists(save_file))
  get_ons_series(2015:2020, index = "CPIH", save_path = save_dir)
  expect_true(file.exists(save_file))
})
