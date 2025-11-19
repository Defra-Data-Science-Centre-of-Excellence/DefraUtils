#' @title Internal RStudio addins to open new scripts with suggested templates
#'
#' @author Josh Moatt
#'
#' @description These functions are designed to be used only via the RStudio
#'   Addins menu. They open a new R script pre-populated with either:
#' \itemize{
#'   \item A suggested script header
#'   \item A Roxygen documentation skeleton
#' }
#'
#'   They use [rstudioapi::documentNew()] to open a new script pane in RStudio.
#'   Calling these functions directly will not work correctly outside the
#'   RStudio IDE.
#'
#' @details These addins are part of the DefraUtils RStudio integration and are
#'   intended to promote consistent documentation practices across projects.
#'   They are not designed to be used interactively from the console.
#'
#' @return Opens a new R script in RStudio; returns `NULL` invisibly.
#'
#' @note These functions are triggered through the **RStudio Addins** menu and
#'   should not be called manually.
#'
#' @seealso [rstudioapi::documentNew()]
#'
#' @keywords internal
#'
#' @name script_addins
NULL


#' @rdname script_addins
#' @export
defra_script_addin <- function() {
  # make FS header template
  header <- stringr::str_c(
    "## - - - - - - - - - - - - - - \n",
    "## \n",
    "## Organisation:  \n",
    "## \n",
    "## Project: \n",
    "## \n",
    "## Script name: \n",
    "## \n",
    "## Purpose of script: \n",
    "## \n",
    "## Author: \n",
    "## \n",
    "## Email: \n",
    "## \n",
    "## Date Created: ", format(Sys.Date(), "%d/%m/%Y"), " \n",
    "## \n",
    "## - - - - - - - - - - - - - - \n",
    "## Notes: \n",
    "## \n",
    "## \n",
    "## - - - - - - - - - - - - - - \n",
    "## Packages: \n",
    "\n",
    "# Pacman \n",
    "if (!require(pacman)) { \n",
    "  install.packages(\"pacman\") \n",
    "  library(pacman) # for automatic install of packages through p_load \n",
    "} \n",
    "\n",
    "# Common packages (add additional packages as needed): \n",
    "p_load( \n",
    "  glue, \n",
    "  here, \n",
    "  openxlsx, \n",
    "  readODS, \n",
    "  tidyverse \n",
    ") \n",
    "\n",
    "## - - - - - - - - - - - - - - \n",
    "## Sourced files: \n",
    "\n",
    "## - - - - - - - - - - - - - - \n"
  )

  # Open using Rstudio API
  rstudioapi::documentNew(
    text = header,
    type = "r",
    execute = FALSE,
    position = 43
  )
}

#' @rdname script_addins
#' @export
defra_roxygen_addin <- function() {
  # make FS header template
  header <- stringr::str_c(
    "#' @title \n",
    "#' Title \n",
    "#' \n",
    "#' @author \n",
    "#' Name (email address) \n",
    "#' \n",
    "#' @description \n",
    "#' Description of function \n",
    "#' \n",
    "#' @details \n",
    "#' Details of function (optional) \n",
    "#' \n",
    "#' @param argument description add more as needed \n",
    "#' \n",
    "#' @param argument description add more as needed \n",
    "#' \n",
    "#' @return \n",
    "#' what is the output \n",
    "#' \n",
    "#' @examples",
    "#' Example of how to run function",
    "#' \n",
    "#' @export \n"
  )

  # Open using Rstudio API
  rstudioapi::documentNew(
    text = header,
    type = "r",
    execute = FALSE,
    position = 22
  )
}
