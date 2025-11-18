#' @title Prompt user to set Databricks Personal Access Token (PAT)
#'
#' @author Josh Moatt
#'
#' @description Prompts the user to enter their Databricks Personal Access Token
#'   (PAT) interactively. This avoids hardcoding sensitive credentials in
#'   scripts, reducing the risk of accidentally committing them to version
#'   control.
#'
#'   This function is useful when working on the Defra DASH platform with the
#'   `brickster` package. It is not required on Posit Workbench, which handles
#'   credentials internally, and does not work in Databricks notebooks.
#'
#' @return A character string containing the entered PAT. Typically assigned to
#'   an environment variable using \code{Sys.setenv(DATABRICKS_TOKEN = token)}.
#'
#' @examples
#' \dontrun{
#' # set PAT
#' token <- set_databricks_pat()
#'
#' # set to environmental variables
#' Sys.setenv(DATABRICKS_TOKEN = token)
#' }
#'
#' @export
set_databricks_pat <- function() {
  tmp <- readline("Please enter your databricks PAT: \n")

  cli::cli_alert_success("Databricks PAT set to: {tmp}")
  cli::cli_text("")

  # return
  tmp
}
