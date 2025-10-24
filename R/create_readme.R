#' @title
#' Create a README
#'
#' @author Josh Moatt
#'
#' @description
#' Use this function to create a README for an RStudio project.
#'
#' @details
#' This function will create a README for a project using a generic template.
#' The function will create a Quarto (.qmd) file and do a first render producing
#'  the initial output.
#'
#' The output type can be controlled using the format option (default is
#' markdown). Can also be html or github (gfm).
#'
#' @param format controls the output format of the README. Default is
#'   "markdown", but can be "html" or "github".
#'
#' @param file_path sting containing file path where README will be saved.
#'
#' @param author string containing authors name. If no string provided author
#'   will be set to "add author".
#'
#' @param readme_title string containing README title. If no string provided
#'   will be set to "README (edit title)".
#'
#' @return
#' Output is a .qmd file containing the desired README template and an initial
#' render of the README in the desired output.
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
