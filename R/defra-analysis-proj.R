#' @title Internal function for Defra Analysis RStudio project template
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description This function is called automatically by RStudio when a user
#' creates a new project using the **Defra Analysis Project** template. It
#' should **not** be called directly by users and will not run correctly outside
#' the RStudio “New Project” interface.
#'
#' @details This function creates the folder structure and default files for a
#' Defra analysis project. It is included in the package only to support
#' RStudio's project template mechanism and is not intended for manual use.
#'
#' @param path Character string. The file path where the new project should be
#'   created.
#' @param ... Additional arguments passed automatically from the RStudio
#'   template system.
#'
#' @return Invisibly returns `NULL`. Called for its side effects of creating the
#'   project directory structure and default files.
#'
#' @note This function is designed for **internal use only** and is
#' automatically triggered by RStudio. Manual use is unsupported.
#'
#' @keywords internal
#'
#' @noRd
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
