test_that("set_console_prompt errors on invalid scope", {
  expect_error(
    set_console_prompt("invalid"),
    regexp = "Invalid scope selected"
  )
})

test_that("set_console_prompt detects existing prompt and aborts", {
  # Stub readLines to simulate existing prompt
  mockery::stub(
    set_console_prompt,
    "readLines",
    function(...) {
      c(".First <- function()", "my_prompt <- function()")
    }
  )

  mockery::stub(
    set_console_prompt,
    "cli::cli_alert_danger",
    function(...) NULL
  )
  mockery::stub(
    set_console_prompt,
    "cli::cli_alert_info",
    function(...) NULL
  )

  expect_error(
    set_console_prompt("project"),
    regexp = "Prompt already present"
  )
})

test_that("set_console_prompt writes prompt to correct file", {
  captured <- NULL

  # Stub readLines to simulate no existing prompt
  mockery::stub(
    set_console_prompt,
    "readLines",
    function(...) character(0)
  )

  # Stub cat to capture written content
  mockery::stub(
    set_console_prompt,
    "cat",
    function(content, file, append) {
      captured <<- list(
        content = content,
        file = file,
        append = append
      )
      NULL
    }
  )

  mockery::stub(
    set_console_prompt,
    "cli::cli_alert_info",
    function(...) NULL
  )

  set_console_prompt("project")

  expect_true(grepl(".First <- function\\(", captured$content))
  expect_equal(captured$file, "./.Rprofile")
  expect_true(captured$append)
})
