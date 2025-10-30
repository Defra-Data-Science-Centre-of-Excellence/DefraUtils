test_that("uc_volume_get stops for failed response", {
  mockery::stub(
    uc_volume_get,
    "httr::GET",
    function(...) {
      structure(list(status_code = 403), class = "response")
    }
  )

  # Expect stop_for_status to be called and error thrown
  mockery::stub(
    uc_volume_get,
    "httr::stop_for_status",
    function(response) {
      stop("Forbidden")
    }
  )

  expect_error(
    uc_volume_get("https://example.com", "vol", "token", "file.txt"),
    regexp = "Forbidden"
  )
})
