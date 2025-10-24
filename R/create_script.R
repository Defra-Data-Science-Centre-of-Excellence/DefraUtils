#' @title
#' Create a new script with the default R script template.
#'
#' @author Josh Moatt
#'
#' @description
#' This function will create a new script usnig the default R script template.
#' There a multiple options for customisation (see details).
#'
#' @details
#' This is a simple function to create a new script using the default R script
#' template.
#'
#' By just calling the function a script will be created with the template
#' entered. All fields of the template will be blank except for the date, which
#' is set to today's date.
#'
#' There are options for customising and entering some details automatically.
#' The details that can be pre-entered are:
#'
#' \itemize{
#'    \item script name
#'    \item location to save
#'    \item author
#'    \item author's email
#'    \item date created
#' }
#'
#' By default these are set to NULL (except for the date). But can be set to any
#' string.
#'
#' @param file_name string containing desired name for script.
#'
#' @param file_path string containing folder name to save script. This is built
#'   on the here function in R, so will follow your root directory. If you want
#'   to save in a sub-folder, enter the full folder sequence, e.g.
#'   "folder/sub-folder".
#'
#' @param author string containing author's name.
#'
#' @param email string containing author's email.
#'
#' @param date string containing a date. By default, this will be set as today's
#'   date.
#'
#' @return
#' An R script will be saved in the root directory or in the specified folder.
#'
#' @export
create_script <- function(
  file_name = NULL,
  file_path = NULL,
  author = NULL,
  email = NULL,
  date = format(
    Sys.Date(),
    "%d/%m/%Y"
  )
) {
  # header
  header <- stringr::str_c(
    "## - - - - - - - - - - - - - -\n",
    "##\n",
    "## Organisation: \n",
    "##\n",
    "## Project:\n",
    "##\n",
    "## Script name: \n",
    "##\n",
    "## Purpose of script: \n",
    "##\n",
    "## Author: ", author, "\n",
    "##\n",
    "## Email: ", email, "\n",
    "##\n",
    "## Date Created: ", date, "\n",
    "##\n",
    "## - - - - - - - - - - - - - -\n",
    "## Notes:\n",
    "##\n",
    "##\n",
    "## - - - - - - - - - - - - - -\n",
    "## Packages:\n",
    "\n",
    "# Pacman\n",
    "if(!require(pacman)){\n",
    "  install.packages(\"pacman\")\n",
    "  library(pacman) # for automatic install of packages through p_load\n",
    "}\n",
    "\n",
    "# Common packages (add additional packages as needed):\n",
    "p_load(\n",
    "  glue,\n",
    "  here,\n",
    "  openxlsx,\n",
    "  readODS,\n",
    "  tidyverse\n",
    ")\n",
    "\n",
    "## - - - - - - - - - - - - - -\n",
    "## Functions:\n",
    "\n",
    "## - - - - - - - - - - - - - -\n",
    "\n"
  )

  # file name
  name_script <- ifelse(
    is.null(file_name),
    "New Script",
    file_name
  )

  # file path
  add_to <- ifelse(
    is.null(file_path),
    here::here(glue::glue("{name_script}.R")),
    here::here(glue::glue("{file_path}"), glue::glue("{name_script}.R"))
  )

  # create qmd
  cat(
    header,
    file = add_to,
    sep = "\n"
  )
}
