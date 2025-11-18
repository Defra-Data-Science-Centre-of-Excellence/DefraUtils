test_that("Invalid format throws error", {
  expect_error(
    create_script_template(format = "invalid"), 
    regexp = "Invalid format"
  )
})


test_that("create_script_template creates default template in dash path", {
  # create local home file
  temp_home <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_home)
  
  # generate template script
  create_script_template(
    format = "default", 
    dash = TRUE
  )
  
  # create expected path
  expected_path <- file.path(
    temp_home, 
    ".config", 
    "rstudio", 
    "templates", 
    "default.R"
  )
  
  # check file exists
  expect_true(
    file.exists(expected_path)
  )
  
  # check template applied
  test_file <- readLines(expected_path)
  expect_true(any(grepl("## Organisation:", test_file)))
  expect_true(any(grepl("## Email:", test_file)))
  expect_true(any(grepl("## Packages:", test_file)))
})

test_that("Custom format requires template", {
  expect_error(
    create_script_template(
      format = "custom", 
      template = NULL
    ), 
    regexp = "no template provided"
  )
})


test_that("create_script_template with custom format writes template", {
  # set temporary home
  temp_home <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_home)
  
  # create custom template and run function
  custom_template <- c("# Custom Header", "# Author: Josh")
  create_script_template(
    format = "custom", 
    template = custom_template, 
    dash = TRUE
  )
  
  # check file exists and custom template applied
  expected_path <- file.path(
    temp_home, 
    ".config", 
    "rstudio", 
    "templates", 
    "default.R"
  )
  expect_true(file.exists(expected_path))
  expect_equal(
    readLines(expected_path), 
    custom_template
  )
})

test_that("create_script_template with blank format deletes file", {
  # set temporary home directory
  temp_home <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_home)
  
  # set file path
  file_path <- file.path(
    temp_home, 
    ".config", 
    "rstudio", 
    "templates"
  )
  
  # create dir and file (need to us fs)
  fs::dir_create(file_path)
  fs::file_create(
    file.path(file_path, "default.R")
  )
  
  # check default.R exists
  expect_true(
    file.exists(file.path(file_path, "default.R"))
  )
  
  # run function as blank
  create_script_template(
    format = "blank", 
    dash = TRUE
  )
  
  # check false
  expect_false(
    file.exists(file.path(file_path, "default.R"))
  )
})
