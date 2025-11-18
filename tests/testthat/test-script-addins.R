test_that("defra_script_addin calls rstudioapi::documentNew correctly", {
  captured_args <- list()

  # Stub rstudioapi::documentNew to capture arguments
  mockery::stub(
    defra_script_addin,
    "rstudioapi::documentNew",
    function(text, type, execute, position) {
      captured_args <<- list(
        text = text,
        type = type,
        execute = execute,
        position = position
      )
      NULL
    }
  )

  defra_script_addin()

  # Check that the header contains expected elements
  expect_true(grepl("Organisation:", captured_args$text))
  expect_true(grepl("Date Created:", captured_args$text))
  expect_equal(captured_args$type, "r")
  expect_false(captured_args$execute)
  expect_equal(captured_args$position, 43)
})

test_that("defra_roxygen_addin calls rstudioapi::documentNew correctly", {
  captured_args <- list()

  # Stub rstudioapi::documentNew to capture arguments
  mockery::stub(
    defra_roxygen_addin,
    "rstudioapi::documentNew",
    function(text, type, execute, position) {
      captured_args <<- list(
        text = text,
        type = type,
        execute = execute,
        position = position
      )
      NULL
    }
  )

  defra_roxygen_addin()

  # Check that the header contains expected elements
  expect_true(grepl("@title", captured_args$text))
  expect_true(grepl("@param", captured_args$text))
  expect_equal(captured_args$type, "r")
  expect_false(captured_args$execute)
  expect_equal(captured_args$position, 17)
})
