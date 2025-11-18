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

test_that("requesting a snapshot with a date works", {
  expect_no_error(get_ons_series(2015:2020, "GDP", get_snapshot = T, snapshot_date = "2025-06-30"))
})

test_that("requesting a snapshot with the wrong date gives an error", {
  expect_error(get_ons_series(2015:2020, "GDP", get_snapshot = T, snapshot_date = "2025-07-30"),
               paste0("The chosen snapshot_date was not found in the list of snapshots\n",
               "Please check available dates at https://www.ons.gov.uk/economy/",
               "grossdomesticproductgdp/timeseries/ybgb/ukea/previous"))
})

test_that("requesting a snapshot without a date prompts the user to choose one", {

  expected_message <- paste0("Enter a number to select a snapshot date from the list\n",
                             "Note that only the first 10 snapshots are shown\n",
                             "To get an older snapshot, find the URL at ",
                             "https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/ybgb/ukea/previous",
                             "\nOr specify a date in the snapshot_date argument")

  with_mocked_bindings(
    code = {
      expect_message(get_ons_series(2015:2020, "GDP", get_snapshot = T), expected_message)
    },
    get_user_input = function(prompt) {1}
  )

})

test_that("creates specified file", {
  save_dir <- tempdir()
  save_file <- file.path(save_dir, "ons_series_l522_mm23.csv")
  on.exit(unlink(save_file))
  expect_false(file.exists(save_file))
  get_ons_series(2015:2020, index = "CPIH", save_path = save_dir)
  expect_true(file.exists(save_file))
})
