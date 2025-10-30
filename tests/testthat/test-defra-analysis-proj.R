test_that("defra_analysis_proj creates expected structure", {
  temp_dir <- tempdir()

  # Stub DefraUtils functions
  mockery::stub(
    defra_analysis_proj,
    "DefraUtils::create_readme",
    function(...) NULL
  )
  mockery::stub(
    defra_analysis_proj,
    "DefraUtils::create_script",
    function(...) NULL
  )

  defra_analysis_proj(temp_dir)

  # Check main project folder and subfolders
  expect_true(dir.exists(temp_dir))
  expect_true(dir.exists(file.path(temp_dir, "data")))
  expect_true(dir.exists(file.path(temp_dir, "src")))
  expect_true(dir.exists(file.path(temp_dir, "outputs")))
})

test_that("defra_analysis_proj sets default author and title", {
  temp_dir <- tempdir()

  # Capture arguments passed to create_readme
  captured <- list()
  mockery::stub(
    defra_analysis_proj,
    "DefraUtils::create_readme",
    function(format, file_path, author, readme_title) {
      captured <<- list(
        format = format,
        file_path = file_path,
        author = author,
        title = readme_title
      )
      NULL
    }
  )

  mockery::stub(
    defra_analysis_proj,
    "DefraUtils::create_script",
    function(...) NULL
  )

  defra_analysis_proj(
    temp_dir,
    readme = "md",
    author = "",
    title = ""
  )

  expect_equal(captured$author, "Author name")
  expect_equal(captured$title, "Project title")
})

test_that("defra_analysis_proj passes custom author and title", {
  temp_dir <- tempdir()

  captured <- list()
  mockery::stub(
    defra_analysis_proj,
    "DefraUtils::create_readme",
    function(format, file_path, author, readme_title) {
      captured <<- list(
        format = format,
        file_path = file_path,
        author = author,
        title = readme_title
      )
    NULL
    }
  )

  mockery::stub(
    defra_analysis_proj,
    "DefraUtils::create_script",
    function(...) NULL
  )

  defra_analysis_proj(
    temp_dir,
    readme = "md",
    author = "Josh Moatt",
    title = "My Project"
  )

  expect_equal(captured$author, "Josh Moatt")
  expect_equal(captured$title, "My Project")
})

