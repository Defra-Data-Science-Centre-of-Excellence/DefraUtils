test_that("uc_volume_put runs without error", {
  # Stub all httr2 calls to return dummy request/response objects
  dummy_request <- structure(list(), class = "httr2_request")
  dummy_response <- structure(list(status_code = 200), class = "httr2_response")

  mockery::stub(
    uc_volume_put,
    "httr2::request", function(...) {
      dummy_request
    }
  )
  mockery::stub(
    uc_volume_put,
    "httr2::req_method",
    function(...) dummy_request
  )
  mockery::stub(
    uc_volume_put,
    "httr2::req_headers",
    function(...) dummy_request
  )
  mockery::stub(
    uc_volume_put,
    "httr2::req_body_file",
    function(...) {
      dummy_request
    }
  )
  mockery::stub(
    uc_volume_put,
    "httr2::req_perform",
    function(...) dummy_response
  )

  expect_silent(
    uc_volume_put(
      workspace = "https://example.com",
      volume = "my_volume",
      token = "abc123",
      file = "data.csv",
      folder = "my_folder"
    )
  )
})

test_that("uc_volume_put stops for failed response", {
  dummy_request <- structure(list(), class = "httr2_request")

  # Stub the request pipeline
  mockery::stub(
    uc_volume_put,
    "httr2::request",
    function(...) dummy_request
  )
  mockery::stub(
    uc_volume_put,
    "httr2::req_method",
    function(...) dummy_request
  )
  mockery::stub(
    uc_volume_put,
    "httr2::req_headers",
    function(...) dummy_request
  )
  mockery::stub(
    uc_volume_put,
    "httr2::req_body_file",
    function(...) dummy_request
  )

  # Stub req_perform to simulate a failed response
  mockery::stub(
    uc_volume_put,
    "httr2::req_perform",
    function(req) {
      stop("Upload failed")
    }
  )

  expect_error(
    uc_volume_put(
      workspace = "https://example.com",
      volume = "my_volume",
      token = "abc123",
      file = "data.csv",
      folder = "my_folder"
    ),
    regexp = "Upload failed"
  )
})
