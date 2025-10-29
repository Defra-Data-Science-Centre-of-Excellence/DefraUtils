test_that("create_readme creates a README.qmd with correct content", {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Run the function
  create_readme(
    format = "markdown",
    file_path = temp_dir,
    author = "Josh Moatt",
    readme_title = "Test README"
  )
  
  # Path to the README
  readme_path <- file.path(temp_dir, "README.qmd")
  
  # Check file exists
  expect_true(file.exists(readme_path))
  
  # Read content
  content <- readLines(readme_path)
  
  # Check for expected strings
  expect_true(any(grepl("title: Test README", content)))
  expect_true(any(grepl("author: Josh Moatt", content)))
  expect_true(any(grepl("markdown: default", content)))
})

test_that("create_readme creates a README.md with correct content", {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Run the function
  create_readme(
    format = "markdown",
    file_path = temp_dir,
    author = "Josh Moatt",
    readme_title = "Test README"
  )
  
  # Path to the README
  readme_path <- file.path(temp_dir, "README.md")
  
  # Check file exists
  expect_true(file.exists(readme_path))
  
  # Read content
  content <- readLines(readme_path)

  # Check for expected strings
  expect_true(any(grepl("Test README", content)))
  expect_true(any(grepl("Josh Moatt", content)))
  expect_true(any(grepl("## Introduction", content)))
})

test_that("create_readme creates a gfm README.md with correct content", {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Run the function
  create_readme(
    format = "github",
    file_path = temp_dir,
    author = "Josh Moatt",
    readme_title = "Test README"
  )
  
  # Path to the README
  readme_path <- file.path(temp_dir, "README.md")
  
  # Check file exists
  expect_true(file.exists(readme_path))
  
  # Read content
  content <- readLines(readme_path)
  
  # Check for expected strings
  expect_true(any(grepl("# Test README", content)))
  expect_true(any(grepl("Josh Moatt", content)))
  expect_true(any(grepl("## Introduction", content)))
})

test_that("create_readme creates a GFM README.md with correct content", {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Run the function
  create_readme(
    format = "html",
    file_path = temp_dir,
    author = "Josh Moatt",
    readme_title = "Test README"
  )
  
  # Path to the README
  readme_path <- file.path(temp_dir, "README.html")
  
  # Check file exists
  expect_true(file.exists(readme_path))
})
