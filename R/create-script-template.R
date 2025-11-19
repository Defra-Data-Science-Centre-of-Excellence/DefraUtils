#' @title Create a default R script template
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description Creates a default R script template to be applied as a header to
#'   all newly opened scripts. Supports default, custom, manual, and blank
#'   templates to help enforce consistent documentation practices.
#'
#' @details This function sets a default header template for new R scripts,
#'   helping enforce best practices in documentation. Depending on the
#'   environment, the template is saved in:
#' \itemize{
#'   \item Local RStudio: \code{~/AppData/Roaming/RStudio/templates}
#'   \item DASH platform: \code{~/.config/rstudio/templates}
#' }
#'
#'   The function supports four modes:
#' \itemize{
#'   \item \code{"default"}: Applies a suggested header template with key metadata.
#'   \item \code{"custom"}: Applies a user-defined template provided as a string.
#'   \item \code{"manual_edit"}: Opens the template file for manual editing.
#'   \item \code{"blank"}: Removes the template, reverting to blank scripts.
#' }
#'
#' @param format Character. One of \code{"default"}, \code{"custom"},
#'   \code{"manual_edit"}, or \code{"blank"}. Determines the type of template to
#'   apply.
#'
#' @param template Character vector or NULL. Required if \code{format =
#'   "custom"}. Specifies the custom template content.
#'
#' @param dash Logical. Default is \code{FALSE}. If \code{TRUE}, uses the DASH
#'   platform path.
#'
#' @return A new .R file containing the script template is created or modified.
#'
#' @examples
#' \dontrun{
#' # Set to default template on local install
#' create_script_template()
#'
#' # create custom template
#' my_template <- c(
#'   "## Script name: ",
#'   "##",
#'   "## Purpose of script: ",
#'   "##",
#'   "## Author: ",
#'   "##",
#'   "## Date Created: "
#' )
#'
#' # apply custom template in dash
#' create_script_template(
#'   format = "custom",
#'   template = my_template,
#'   dash = TRUE
#' )
#'
#' # remove template
#' create_script_template(format = "blank")
#' }
#'
#' @export
create_script_template <- function(
  format = "default",
  template = NULL,
  dash = FALSE
) {
  # check format choice
  if (!format %in% c("default", "custom", "manual_edit", "blank")) {
    cli::cli_alert_danger("Error: invalid format selected")
    cli::cli_alert_info("Please set format to one of:")
    cli::cli_bullets(c(
      "*" = '"default"',
      "*" = '"custom"',
      "*" = '"manual_edit"',
      "*" = '"blank"'
    ))
    stop("Invalid format")
  }

  # set file path
  file_path <- ifelse(
    dash,
    "~/.config/rstudio/templates",
    "~/AppData/Roaming/RStudio/templates"
  )

  # Create header based on format
  if (format == "default") {
    ## default

    # create template file if it doesn't exist already
    if (!fs::file_exists(file_path)) {
      fs::dir_create(path = file_path)
    }

    # make default header template
    header <- c(
      "## - - - - - - - - - - - - - -",
      "##",
      "## Organisation:",
      "##",
      "## Project:",
      "##",
      "## Script name: ",
      "##",
      "## Purpose of script: ",
      "##",
      "## Author: ",
      "##",
      "## Email: ",
      "##",
      "## Date Created: ",
      "##",
      "## - - - - - - - - - - - - - -",
      "## Notes:",
      "##",
      "##",
      "## - - - - - - - - - - - - - -",
      "## Packages:",
      "",
      "# Pacman",
      "if (!require(pacman)) {",
      "  install.packages(\"pacman\")",
      "  library(pacman) # for automatic install of packageds through p_load",
      "}",
      "",
      "# Packages (add additional packages as needed):",
      "p_load(",
      "  glue,",
      "  here,",
      "  openxlsx,",
      "  readODS,",
      "  tidyverse",
      ")",
      "",
      "## - - - - - - - - - - - - - -",
      "## Sourced files/functions:",
      "",
      "## - - - - - - - - - - - - - -",
      ""
    )

    # Create default R script template with header
    cat(
      header,
      file = fs::path_expand(paste0(file_path, "/default.R")),
      sep = "\n"
    )
  } else if (format == "custom") {
    ## custom

    # check template provided
    if (is.null(template)) {
      cli::cli_alert_danger("No template provided - aborting!")
      stop("Error: no template provided.")
    }

    # create template file if it doesn't exist already
    if (!fs::file_exists(file_path)) {
      fs::dir_create(path = file_path)
    }

    # Create default R script template with header provided
    cat(
      template,
      file = fs::path_expand(paste0(file_path, "/default.R")),
      sep = "\n"
    )
  } else if (format == "manual_edit") {
    ## edit template manually

    # create template file if it doesn't exist already
    if (!fs::file_exists(file_path)) {
      fs::dir_create(path = file_path)
    }

    # Create the default file if doesn't exist already
    if (!fs::file_exists(paste0(file_path, "/default.R"))) {
      fs::file_create(paste0(file_path, "/default.R"))
    }

    # Open the file in RStudio to edit it
    usethis::edit_file(paste0(file_path, "/default.R"))
  } else if (format == "blank") {
    ## remove template

    # this removes any pre-existing defaults
    fs::file_delete(paste0(file_path, "/default.R"))
  }
}
