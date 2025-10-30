test_that("set_rstudio_layout updates preferences correctly", {
  # Simulate current preferences
  mockery::stub(
    set_rstudio_layout,
    "rstudioapi::readRStudioPreference",
    function(name, default) {
      "old_value"
    }
  )

  # Capture written preferences
  written <- list()
  mockery::stub(
    set_rstudio_layout,
    "rstudioapi::writeRStudioPreference",
    function(name, value) {
      written[[name]] <<- value
      NULL
    }
  )

  # Run function with test preferences
  set_rstudio_layout(
    pane_layout = "custom",
    console_width = 80
  )

  # Check that preferences were written correctly
  expect_equal(written$pane_layout, "custom")
  expect_equal(written$console_width, 80)
})
