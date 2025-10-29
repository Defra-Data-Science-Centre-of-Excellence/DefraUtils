test_that("create_dash_dir succeeds on first try", {
  # Stub brickster call to succeed immediately
  mockery::stub(
    create_dash_dir,
    "brickster::db_volume_dir_create",
    function(...) NULL
  )
  mockery::stub(
    create_dash_dir,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    create_dash_dir("some_path")
  )
})

test_that("create_dash_dir retries on failure and then succeeds", {
  call_count <- 0
  mockery::stub(
    create_dash_dir,
    "brickster::db_volume_dir_create",
    function(...) {
      call_count <<- call_count + 1
      if (call_count < 3) stop("Temporary failure")
      return(NULL)
    }
  )

  mockery::stub(
    create_dash_dir,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    create_dash_dir("some_path")
  )
  expect_equal(call_count, 3)
})

test_that("create_dash_dir fails after max retries", {
  mockery::stub(
    create_dash_dir,
    "brickster::db_volume_dir_create",
    function(...) {
      stop("Always fails")
    }
  )

  mockery::stub(
    create_dash_dir,
    "Sys.sleep",
    function(...) NULL
  )

  expect_error(
    create_dash_dir(
      "some_path",
      max_tries = 3
    ),
    regexp = "Failed to write file after 3 attempts"
  )
})
