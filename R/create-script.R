#' @title Create a new script using the default template.
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description This function will create a new R script using the default
#'   header template. There are multiple options for customisation (see
#'   details).
#'
#' @details This function generates a new R script pre-filled with a standard
#'   header template. By default, all fields are left blank except for the
#'   creation date, which is set to today's date.
#'
#'   You can optionally pre-fill the following fields:
#' \itemize{
#'   \item Script name
#'   \item Save location
#'   \item Author name
#'   \item Author email
#'   \item Date created
#' }
#'
#'   The script is saved in your project's root directory (via [here::here()])
#'   or a specified sub-folder.
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
#' @return An R script will be saved in the root directory or in the specified
#'   folder.
#'
#' @examples
#' \dontrun{
#' # Create a script with default settings
#' create_script()
#'
#' # Create a script with custom metadata
#' create_script(
#'   file_name = "analysis_script",
#'   file_path = "scripts",
#'   author = "Farming Stats",
#'   email = "Farming.Stats@defra.gov.uk"
#' )
#' }
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
  # author
  author_name <- ifelse(
    is.null(author),
    "Name",
    author
  )

  # email
  email_addr <- ifelse(
    is.null(email),
    "email",
    email
  )

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
    "## Author: ", author_name, "\n",
    "##\n",
    "## Email: ", email_addr, "\n",
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
