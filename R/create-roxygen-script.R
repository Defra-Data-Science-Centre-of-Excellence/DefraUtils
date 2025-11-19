#' @title Create a new script using the default template.
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description This function will create a new R script with a roxygen2
#'   template for use documenting functions. There are multiple options for
#'   customisation (see details).
#'
#' @details This function generates a new R script pre-filled with a standard
#'   roxygen2 template.
#'
#'   You can optionally pre-fill the following fields:
#' \itemize{
#'   \item Script name
#'   \item Save location
#'   \item Author name
#'   \item Author email
#' }
#'
#'   The script is saved in your project's root directory (via [here::here()])
#'   or a specified folder.
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
#' @return An R script will be saved in the root directory or in the specified
#'   folder.
#'
#' @examples
#' \dontrun{
#' # Create a script with default settings
#' create_roxygen_script()
#'
#' # Create a script with custom metadata
#' create_roxygen_script(
#'   file_name = "my_function_name",
#'   file_path = "R",
#'   author = "Farming Stats",
#'   email = "Farming.Stats@defra.gov.uk"
#' )
#' }
#'
#' @export
create_roxygen_script <- function(
  file_name = NULL,
  file_path = NULL,
  author = NULL,
  email = NULL
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
    "#' @title \n",
    "#' Title \n",
    "#' \n",
    "#' @author \n",
    "#' ", author_name, " ([", email_addr, "](mailto:", email_addr, ")) \n",
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
    "#' @examples \n",
    "#' Example of how to run function \n",
    "#' \n",
    "#' @export \n"
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
