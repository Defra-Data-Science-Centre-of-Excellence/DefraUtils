#' @title Addin to open new scripts with suggested templates
#'
#' @description There are two addins that will open a new blank R script which
#' is populated with one of two suggested script headers. The script addin will
#' open with our suggested script header, which contains key meta-data to help
#' you document your script properly. The roxygen addin will add the roxygen
#' skeleton to properly document functions. Although roxygen comments are mostly
#' used for documenting funtions in packages, we suggest using them to document
#' any function you create, as this will inevetably make it easier for others to
#' pick up.
#'
#' @return A script will open in RStudio.
#'
#' @name script_addins
#'
#' @export
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
    "  library(pacman) # for automatic install of missing packaged through p_load \n",
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
    "#' @export \n"
  )

  # Open using Rstudio API
  rstudioapi::documentNew(
    text = header,
    type = "r",
    execute = FALSE,
    position = 17
  )
}
