#' @title Create an default R script template.
#'
#' @author Josh Moatt
#'
#' @description This function can be used to create a default R script template.
#'   All scripts opened from this point will have this template applied as a
#'   header. It comes pre-loaded with a suggested template, but can also handle
#'   a custom template be manually edited, or returned to a blank script. The
#'   default template contains a header with key metadata to help you document
#'   your scripts properly.
#'
#' @details This function is used to create a new template for R scripts. Once
#'   run, all newly opened scripts will have you template/header by default.By
#'   doing this is it should be easier to follow best practice and properly
#'   document all the scripts you create.
#'
#'   The template will be saved in different locations depending on where you
#'   are working.  When working on local installs of RStudio, the template will
#'   be stored on your c drive "~/AppData/Roaming/RStudio". When working on the
#'   DASH platform, the template will be saved in the .config folder in your
#'   account/profile "/.config/rstudio". The function will create a "templates"
#'   folder where the default will be stored.
#'
#'   The function has various ways it can work which will give you the ability
#'   to create whatever header template you wish. It has four ways formatting
#'   options:
#'
#' \itemize{
#'    \item default
#'    \item custom
#'    \item manual_edit
#'    \item blank
#' }
#'
#'   "default" will pre-load the the template with our suggested script header.
#'   This header includes all the key meta-data sections to help you properly
#'   document your scripts.
#'
#'   "custom" will allow you to provide your own custom template as a string,
#'   which will then be added to the template.
#'
#'   "manual" will create/open the template, allowing you to manually edit it.
#'   Once saved, all scripts will open with this default from then on. Note,
#'   this can also be used to tweak a pre-existing template (e.g. to add your
#'   name and email to all scripts).
#'
#'   "blank" will delete the default R script and template. This returns R back
#'   to normal, and any script opened subsequently will be blank.
#'
#' @param format what format the template will take. There are four options:
#'   "default" will apply our suggested template, "custom" will apply a
#'   custom template (provided as a string), "manual_edit" will open the
#'   template so you can manually edit the template, and "blank" can be used to
#'   remove all pre-existing templates. Note: manual edit can also be used to
#'   edit existing templates.
#'
#' @param template default is NULL, only used if format = "custom". Used to
#'   provide custom template design. Must be provided as a string. Must be
#'   provided if using format = "custom" or the function will return an error.
#'
#' @param dash default is FALSE. If TRUE changes the file path to the one needed
#'   for the RStudio server on DASH
#'
#' @return New .R file created containing the script template
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
      "##" ,
      "##",
      "## - - - - - - - - - - - - - -",
      "## Packages:",
      "",
      "# Pacman",
      "if (!require(pacman)) {",
      "  install.packages(\"pacman\")",
      "  library(pacman) # for automatic install of missing packaged through p_load",
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
    if (is.null(template)){
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
