test_that("list_dash_files succeeds on first try", {
  # Stub brickster call to succeed immediately
  mockery::stub(
    list_dash_files,
    "brickster::db_volume_list",
    function(...) NULL
  )
  mockery::stub(
    list_dash_files,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    list_dash_files("some_path")
  )
})

test_that("list_dash_files retries on failure and then succeeds", {
  call_count <- 0
  mockery::stub(
    list_dash_files,
    "brickster::db_volume_list",
    function(...) {
      call_count <<- call_count + 1
      if (call_count < 3) stop("Temporary failure")
      return(NULL)
    }
  )

  mockery::stub(
    list_dash_files,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    list_dash_files("some_path")
  )
  expect_equal(call_count, 3)
})

test_that("list_dash_files fails after max retries", {
  mockery::stub(
    list_dash_files,
    "brickster::db_volume_list",
    function(...) {
      stop("Always fails")
    }
  )
  mockery::stub(
    list_dash_files,
    "Sys.sleep",
    function(...) NULL
  )

  expect_error(
    list_dash_files(
      "some_path",
      max_tries = 3
    ),
    regexp = "Failed to pull list after 3 attempts"
  )
})
