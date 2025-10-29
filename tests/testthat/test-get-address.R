test_url <- "https://www.gov.uk/government/statistics/farm-business-income"

test_that("code with no arguments runs without error", {
  expect_no_error(get_address(test_url))
})

test_that("function can find a CSV", {
  expect_match(get_address(test_url, "csv"),
               "https:\\/\\/assets\\.publishing\\.service\\.gov\\.uk/media/.+\\.csv")
})

test_that("function can find an ODS", {
  expect_match(get_address(test_url, "ods"),
               "https:\\/\\/assets\\.publishing\\.service\\.gov\\.uk/media/.+\\.ods")
})

test_that("function can find a second CSV", {
  expect_false(get_address(test_url, "csv") == get_address(test_url, "csv", 2))
  expect_no_match(get_address(test_url, "csv", 2), "\\/csv-preview\\/.+.csv")
})

test_that("function can find a CSV preview", {
  expect_match(get_address(test_url, "csv", find_csv_preview = T), "\\/csv-preview\\/.+.csv")
})

test_that("search term works", {
  expect_match(get_address(test_url, search_term = "fbs\\.queries"),
               "mailto:fbs.queries@defra.gov.uk")
})
