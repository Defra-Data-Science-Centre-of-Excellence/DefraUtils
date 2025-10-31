#' @title Create a README using a template
#'
#' @author Josh Moatt
#'
#' @description Generates a README file for a project using a generic Quarto
#'   template. The function creates a `.qmd` file and performs an initial render
#'   to produce the output in the specified format.
#'
#' @details This function helps standardize README creation across projects. It
#'   supports output formats \code{"markdown"} (default), \code{"html"}, and
#'   \code{"github"} (GitHub Flavored Markdown, or GFM). The README is saved in
#'   the working directory by default, but a custom location can be specified
#'   using the `file_path` argument.
#'
#'   The template includes sections for project introduction, structure, and
#'   instructions on how to run the project. The rendered output is created
#'   using [quarto::quarto_render()].
#'
#' @param format A string specifying the output format. Options are
#'   \code{"markdown"} (default), \code{"html"}, or \code{"github"}.
#'
#' @param file_path A string specifying the file path where the README should be
#'   saved. Defaults to the working directory.
#'
#' @param author A string specifying the author's name. Defaults to `"add
#'   author"` if not provided.
#'
#' @param readme_title A string specifying the README title. Defaults to "README
#'   (edit title)" if not provided.
#'
#' @return Creates a `.qmd` file and renders it to the specified format. The
#'   output file is saved in the specified location.
#'
#' @examples
#' \dontrun{
#' # create README in active working directory
#' create_readme(
#'   format = "markdown",
#'   author = "Josh Moatt",
#'   readme_title = "My Project README"
#' )
#'
#' # create README in specified location
#' create_readme(
#'   format = "markdown",
#'   file_path = "~/my-project",
#'   author = "Josh Moatt",
#'   readme_title = "My Project README"
#' )
#' }
#'
#' @seealso [quarto::quarto_render()]
#'
#' @export
create_readme <- function(
  format,
  file_path = NULL,
  author = NULL,
  readme_title = NULL
) {
  # output format
  if (format == "github") {
    out_format <- "  gfm: default\n"
  } else if (format == "html") {
    out_format <- stringr::str_c(
      "  html:\n",
      "    self-contained: true\n"
    )
  } else if (format == "markdown") {
    out_format <- "  markdown: default\n"
  } else {
    cli::cli_alert_danger("Invalid format specified")
    cli::cli_alert_info("Set format to: markdown, github or html")
    stop("Invalid format.")
  }

  # author details
  if (!is.null(author)) {
    author <- author
  } else {
    author <- "add author"
  }

  # title
  if (!is.null(readme_title)) {
    readme_title <- readme_title
  } else {
    readme_title <- "README (edit title)"
  }

  # set readme text.
  readme_txt <- stringr::str_c(
    "---\n",
    "title: ", readme_title, "\n",
    "author: ", author, "\n",
    "date: today\n",
    "date-format: \"DD/MM/YYYY\"\n",
    "format:\n",
    out_format,
    "toc: true\n",
    "editor_options:\n",
    "  chunk_output_type: console\n",
    "---\n",
    "\n",
    "## Introduction \n",
    "Introduce you project here. Things to cover in the introduction: \n",
    "\n",
    "* Motivation/purpose of the project\n",
    "* Any key inputs\n",
    "* Where is the project used\n",
    "* What are the key outputs\n",
    "* Where are the outputs used\n",
    "Below are some pre-defined headings which we think it would be useful for you to include.\n",
    "\n",
    "## Project structure \n",
    "Include a description of the project structure here. Outline what each folder contains and how/where it's used. A table can be useful here.\n",
    "\n",
    "## How to run \n",
    "Here explain how to run the project. This should not be a line by line description of the code, rather an overview of how to run the project (what order to run the scripts etc).\n",
    "\n"
  )

  # path to save
  if (!is.null(file_path)) {
    path <- file_path
  } else {
    path <- here::here()
  }

  # create qmd
  cat(
    readme_txt,
    file = glue::glue("{path}/README.qmd"),
    sep = "\n"
  )

  # render Qmd
  quarto::quarto_render(
    glue::glue("{path}/README.qmd"),
    quiet = TRUE
  )
}
