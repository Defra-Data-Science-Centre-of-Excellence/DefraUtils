#' @title Set databricks Persoanl Access Token
#'
#' @author Josh Moatt
#'
#' @description Simple function to set your databricks Personal Access Token
#'   (PAT). Doing this via a function avoids the need to hardcode the PAT in a
#'   script, which helps prevent accidentally committing it to GitHub. Not
#'   needed on Posit Workbench, which handles credentials internally. But useful
#'   for the RStudio server. Note, the function does not work in databricks
#'   notebooks. Useful for when using the `brickster` package. 
#'
#' @return prompt in R console to enter databricks PAT, which can then assigned
#'   to a vector.
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

