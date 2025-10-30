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

test_that("uc_volume_get runs without error", {
  dummy_response <- structure(list(status_code = 200), class = "response")

  # Stub httr::GET to return a dummy response
  mockery::stub(
    uc_volume_get,
    "httr::GET",
    function(...) dummy_response
  )

  # Stub httr::stop_for_status to do nothing
  mockery::stub(
    uc_volume_get,
    "httr::stop_for_status",
    function(response) NULL
  )

  expect_silent(
    uc_volume_get(
      workspace = "https://example.com",
      volume = "my_volume",
      token = "abc123",
      out_file = "output.txt"
    )
  )
})
