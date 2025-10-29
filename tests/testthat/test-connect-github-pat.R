library(mockery)

test_that("Git config commands are constructed correctly", {
  # Stub system calls to avoid actual execution
  mockery::stub(connect_github_pat, "system", function(cmd) {
    expect_true(grepl("git config", cmd))
    return(0)
  })
  
  # Stub gitcreds and credentials calls
  mockery::stub(connect_github_pat, "gitcreds::gitcreds_set", function() NULL)
  mockery::stub(connect_github_pat, "credentials::set_github_pat", function() NULL)
  
  # Stub cli alerts
  mockery::stub(connect_github_pat, "cli::cli_alert_success", function(...) NULL)
  mockery::stub(connect_github_pat, "cli::cli_alert_warning", function(...) NULL)
  
  # Use temp .Rprofile
  temp_dir <- withr::local_tempdir()
  rprofile_path <- file.path(temp_dir, ".Rprofile")
  withr::local_envvar(HOME = temp_dir)
  
  # Run function
  connect_github_pat("testuser", "test@example.com")
  
  # Check .Rprofile was created and contains expected line
  expect_true(file.exists(rprofile_path))
  lines <- readLines(rprofile_path)
  expect_true(any(grepl("credentials::set_github_pat", lines)))
})

test_that("Does not duplicate credentials line in .Rprofile", {
  temp_dir <- withr::local_tempdir()
  rprofile_path <- file.path(temp_dir, ".Rprofile")
  withr::local_envvar(HOME = temp_dir)
  
  # Pre-populate .Rprofile with credentials line
  writeLines("credentials::set_github_pat(verbose = FALSE)", rprofile_path)
  
  # Stub system and interactive calls
  mockery::stub(connect_github_pat, "system", function(cmd) return(0))
  mockery::stub(connect_github_pat, "gitcreds::gitcreds_set", function() NULL)
  mockery::stub(connect_github_pat, "credentials::set_github_pat", function() NULL)
  mockery::stub(connect_github_pat, "cli::cli_alert_success", function(...) NULL)
  mockery::stub(connect_github_pat, "cli::cli_alert_warning", function(...) NULL)
  
  # Run function
  connect_github_pat("testuser", "test@example.com")
  
  # Check no duplicate lines
  lines <- readLines(rprofile_path)
  expect_equal(sum(grepl("credentials::set_github_pat", lines)), 1)
})
  
