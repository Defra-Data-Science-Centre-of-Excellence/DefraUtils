#' @title
#' Function for Defra analysis RStudio project template
#'
#' @author Josh Moatt
#'
#' @description
#' This is a function that is called in the "New project" viewer pane when the
#' user chooses the Defra Analysis Project template. It should not be used away
#' from the RStudio "New project" viewer.
#'
#' I have not included additional information on how to use this function, as it
#' is not intended to be used outside the template call.
#'
#' @export
defra_analysis_proj <- function(path, ...) {
  # ensure path exists
  dir.create(
    path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # collect inputs and paste together as 'Parameter: Value'
  params <- list()
  dots <- list(...)
  for (i in seq_along(dots)) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    params[[key]] <- val
  }

  # set default author if no text entered
  params$author <- ifelse(
    nchar(params$author) < 1,
    "Author name",
    params$author
  )

  # set default title if no text entered
  params$title <- ifelse(
    nchar(params$title) < 1,
    "Project title",
    params$title
  )

  # add readme
  DefraUtils::create_readme(
    format = params$readme,
    file_path = path,
    author = params$author,
    readme_title = params$title
  )

  # add pipeline script
  DefraUtils::create_script(
    file_name = "pipeline",
    file_path = path,
    author = params$author
  )

  # set file structure
  structure <- c("data", "src", "outputs")

  # add folders based on structure
  for (folder in structure) {
    dir.create(
      file.path(path, folder),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

}
