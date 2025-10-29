test_that("Git config commands are constructed correctly", {
  # Stub system calls
  mockery::stub(connect_github_ssh, "system", function(cmd) {
    expect_true(
      grepl("git config", cmd) ||
        grepl("ssh-keygen", cmd) ||
        grepl("cat", cmd) ||
        grepl("ssh-keyscan", cmd) ||
        grepl("ssh -T", cmd)
    )
    return(0)
  })

  # Stub cli alerts
  mockery::stub(
    connect_github_ssh, "cli::cli_text", function(...) NULL
  )
  mockery::stub(
    connect_github_ssh, "cli::cli_alert_success", function(...) NULL
  )
  mockery::stub(
    connect_github_ssh, "cli::cli_alert_info", function(...) NULL
  )
  mockery::stub(
    connect_github_ssh, "cli::cli_alert_danger", function(...) NULL
  )

  # Stub readline to simulate user confirming SSH key added
  mockery::stub(connect_github_ssh, "readline", function(...) "y")

  # Use temp HOME directory
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_dir)

  # Run function
  connect_github_ssh(
    "testuser",
    "test@example.com"
  )

  # Check that .ssh directory was created
  expect_true(
    dir.exists(file.path(temp_dir, ".ssh"))
  )
})

test_that("Handles user not adding SSH key", {
  # Stub system calls
  mockery::stub(connect_github_ssh, "system", function(cmd) {
    return(0)
  })

  # Stub cli alerts
  mockery::stub(
    connect_github_ssh, "cli::cli_text", function(...) NULL
  )
  mockery::stub(
    connect_github_ssh, "cli::cli_alert_success", function(...) NULL
  )
  mockery::stub(
    connect_github_ssh, "cli::cli_alert_info", function(...) NULL
  )
  mockery::stub(
    connect_github_ssh, "cli::cli_alert_danger", function(...) NULL
  )

  # Stub readline to simulate user saying "no"
  mockery::stub(
    connect_github_ssh, "readline", function(...) "n"
  )

  # Use temp HOME directory
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_dir)

  # Run function
  connect_github_ssh(
    "testuser",
    "test@example.com"
  )

  # Check that .ssh directory was created
  expect_true(
    dir.exists(file.path(temp_dir, ".ssh"))
  )
})
