test_that("create_roxygen_script writes expected header", {
  # Create a temporary file path
  temp_dir <- tempdir()
  
  # Run the function
  create_roxygen_script(
    file_name = "test_script",
    file_path = temp_dir,
    author = "Josh Moatt",
    email = "josh@example.com"
  )
  
  # Read the file
  content <- readLines(file.path(temp_dir, "test_script.R"))
  
  # Check that author and email are in the file
  expect_true(any(grepl("Josh Moatt", content)))
  expect_true(any(grepl("josh@example.com", content)))
})

test_that("create_roxygen_script writes expected header when no details provided", {
  # Create a temporary file path
  temp_dir <- tempdir()
  
  # Run the function
  create_roxygen_script(
    file_name = "test_script",
    file_path = temp_dir
  )
  
  # Read the file
  content <- readLines(file.path(temp_dir, "test_script.R"))
  
  # Check that author and email are in the file
  expect_true(any(grepl("Name", content)))
  expect_true(any(grepl("email", content)))
})
