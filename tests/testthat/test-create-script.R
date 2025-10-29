test_that("create_script writes expected header", {
  # Create a temporary file path
  temp_dir <- tempdir()
  
  # Run the function
  create_script(
    file_name = "test_script",
    file_path = temp_dir,
    author = "Josh Moatt",
    email = "josh@example.com",
    date = "29/10/2025"
  )
  
  # Read the file
  content <- readLines(file.path(temp_dir, "test_script.R"))
  
  # Check that author and email are in the file
  expect_true(any(grepl("Author: Josh Moatt", content)))
  expect_true(any(grepl("Email: josh@example.com", content)))
  expect_true(any(grepl("Date Created: 29/10/2025", content)))
})
