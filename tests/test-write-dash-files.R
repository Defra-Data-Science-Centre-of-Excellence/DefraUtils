test_that("dash_volume_write succeeds on first try", {
  # Stub brickster call to succeed immediately
  mockery::stub(
    dash_volume_write,
    "brickster::db_volume_write",
    function(...) NULL
  )
  mockery::stub(
    dash_volume_write,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    dash_volume_write("some_path")
  )
})

test_that("dash_volume_write retries on failure and then succeeds", {
  call_count <- 0
  mockery::stub(
    dash_volume_write,
    "brickster::db_volume_write",
    function(...) {
      call_count <<- call_count + 1
      if (call_count < 3) stop("Temporary failure")
      NULL
    }
  )

  mockery::stub(
    dash_volume_write,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    dash_volume_write("some_path")
  )
  expect_equal(call_count, 3)
})

test_that("read_file_from_volume fails after max retries", {
  mockery::stub(
    dash_volume_write,
    "brickster::db_volume_write",
    function(...) {
      stop("Always fails")
    }
  )

  mockery::stub(
    dash_volume_write,
    "Sys.sleep",
    function(...) NULL
  )

  expect_error(
    dash_volume_write(
      "some_path",
      max_tries = 3
    ),
    regexp = "Failed to read volume after 3 attempts"
  )
})

test_that("write_xlsx_to_volume runs without error", {
  dummy_data <- openxlsx::createWorkbook()

  # Stub saveWorkbook to avoid writing
  mockery::stub(
    write_xlsx_to_volume,
    "openxlsx::saveWorkbook",
    function(data, file) NULL
  )

  # Stub dash_volume_write to avoid volume write
  mockery::stub(
    write_xlsx_to_volume,
    "DefraUtils::dash_volume_write",
    function(...) NULL
  )

  expect_silent(write_xlsx_to_volume(dummy_data, path = "some_path"))
})

test_that("write_rds_to_volume runs without error", {
  dummy_data <- data.frame(x = 1:5)

  # Stub saveRDS to avoid writing
  mockery::stub(
    write_rds_to_volume,
    "saveRDS",
    function(data, file) NULL
  )

  # Stub dash_volume_write
  mockery::stub(
    write_rds_to_volume,
    "DefraUtils::dash_volume_write",
    function(...) NULL
  )

  expect_silent(write_rds_to_volume(dummy_data, path = "some_path"))
})

test_that("write_csv_to_volume runs without error", {
  dummy_data <- data.frame(x = 1:5)

  # Stub write_csv to avoid writing
  mockery::stub(
    write_csv_to_volume,
    "readr::write_csv",
    function(data, file) NULL
  )

  # Stub dash_volume_write
  mockery::stub(
    write_csv_to_volume,
    "DefraUtils::dash_volume_write",
    function(...) NULL
  )

  expect_silent(write_csv_to_volume(dummy_data, path = "some_path"))
})
