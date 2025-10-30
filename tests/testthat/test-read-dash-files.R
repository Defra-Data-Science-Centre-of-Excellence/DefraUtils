test_that("read_file_from_volume succeeds on first try", {
  # Stub brickster call to succeed immediately
  mockery::stub(
    read_file_from_volume,
    "brickster::db_volume_read",
    function(...) NULL
  )
  mockery::stub(
    read_file_from_volume,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    read_file_from_volume("some_path")
  )
})

test_that("read_file_from_volume retries on failure and then succeeds", {
  call_count <- 0
  mockery::stub(
    read_file_from_volume,
    "brickster::db_volume_read",
    function(...) {
      call_count <<- call_count + 1
      if (call_count < 3) stop("Temporary failure")
      NULL
    }
  )

  mockery::stub(
    read_file_from_volume,
    "Sys.sleep",
    function(...) NULL
  )

  expect_silent(
    read_file_from_volume("some_path")
  )
  expect_equal(call_count, 3)
})

test_that("read_file_from_volume fails after max retries", {
  mockery::stub(
    read_file_from_volume,
    "brickster::db_volume_read",
    function(...) {
      stop("Always fails")
    }
  )

  mockery::stub(
    read_file_from_volume,
    "Sys.sleep",
    function(...) NULL
  )

  expect_error(
    read_file_from_volume(
      "some_path",
      max_tries = 3
    ),
    regexp = "Failed to read volume after 3 attempts"
  )
})

test_that("read_csv_from_volume calls read_file_from_volume with .csv", {
  # Create a temporary csv file
  temp_csv <- tempfile(fileext = ".csv")
  writeLines("x,y\n1,2\n3,4", temp_csv)

  # Stub read_file_from_volume to return the temp csv path
  mockery::stub(
    read_csv_from_volume,
    "DefraUtils::read_file_from_volume",
    function(path, ext, max_tries, interval) {
      expect_equal(ext, ".csv")
      temp_csv
    }
  )

  # Stub readr::read_csv to return a known result
  mockery::stub(
    read_csv_from_volume,
    "readr::read_csv",
    function(file, ...) {
      expect_equal(file, temp_csv)
      data.frame(x = c(1, 3), y = c(2, 4))
    }
  )
  # Check returns expected
  result <- read_csv_from_volume("some_path")
  expect_equal(result$x, c(1, 3))
  expect_equal(result$y, c(2, 4))
})

test_that("read_rds_from_volume returns expected object", {
  # dummy list
  expected <- list(
    a = 1,
    b = 2
  )

  # Stub DefraUtils and readr
  mockery::stub(
    read_rds_from_volume,
    "DefraUtils::read_file_from_volume",
    function(...) "fake_path.Rds"
  )
  mockery::stub(
    read_rds_from_volume,
    "readr::read_rds",
    function(path, ...) {
      expect_equal(path, "fake_path.Rds")
      expected
    }
  )

  result <- read_rds_from_volume("some_path")
  expect_equal(result, expected)
})

test_that("read_xlsx_from_volume returns expected object", {
  # dummy data frame
  expected <- data.frame(
    x = 1:3,
    y = letters[1:3]
  )

  # Stub DefraUtils and readxl
  mockery::stub(
    read_xlsx_from_volume,
    "DefraUtils::read_file_from_volume",
    function(...) "fake_path.xlsx"
  )
  mockery::stub(
    read_xlsx_from_volume,
    "readxl::read_xlsx",
    function(path, ...) {
      expect_equal(path, "fake_path.xlsx")
      expected
    }
  )

  result <- read_xlsx_from_volume("some_path")
  expect_equal(result, expected)
})
