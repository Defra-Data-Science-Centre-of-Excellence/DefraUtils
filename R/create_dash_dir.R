#' @title Create a folder in a DASH volume
#'
#' @author Josh Moatt
#'
#' @description Function to streamline creating directories in DASH volumes.
#'   This function will help avoid the http2 error which seems to be a frequent
#'   problem with `brickster`. In order for this function to work, you must be
#'   working on the Defra DASH platform and have set the required `brickster`
#'   environmental variables.
#'
#'
#' @details This function is designed to handle the frequent http2 errors that
#'   occur with `brickster`. From testing, these errors are not code or file
#'   path errors, but are just minor bugs with the API. Often, if the same code
#'   is rerun, the data will be read in fine.
#'
#'   This function uses the `brickster::db_volume_dir_create` function. It uses
#'   a retry loop that catch errors with `tryCatch()`. If no error is thrown by
#'   `brickster`, the function will end as the directory will have been created.
#'   If an error is thrown by `brickster`, the function will wait a set number
#'   of seconds (controlled by the interval argument) before retrying. It will
#'   repeat the attempt create the directory until either the directory is
#'   created successfully or the maximum number of attempts (set by max_tries)
#'   is reached - after which it throws an error.
#'
#'   In order for this function to work, you must be working on the Defra DASH
#'   platform and have set the required brickster environmental variables. These
#'   variables include setting a databricks Personal Access Token (PAT).
#'
#' @param path A string containing the path on DASH catalog for the folder you
#'   wish to create. It should be the full DASH string starting "/Volumes/..."
#'
#' @param ... Additional arguments passed to
#'   `brickster::db_volume_dir_create()`.
#'
#' @param max_tries Maximum number of tries to read in data. Added to deal with
#'   persistent http errors. Default is 5.
#'
#' @param interval Interval between tries to read in data. Added to deal with
#'   persistent http errors. Default is 2
#'
#' @return Folder created in DASH catalog.
#' 
#' @examples
#' \dontrun{
#' # create directory
#' create_dash_dir(
#'   path = "/Volumes/prd_dash_lab/<volume-name>/<new-directory-name>"
#' )
#' }
#'
#' @export
create_dash_dir <- function(..., max_tries = 5, interval = 2) {
  attempt <- 1
  success <- FALSE
  
  while (attempt <= max_tries && !success) {
    tryCatch({
      brickster::db_volume_dir_create(...)
      success <- TRUE
    }, error = function(e) {
      Sys.sleep(interval)
      attempt <<- attempt + 1
    })
  }
  
  if (!success) {
    stop("Failed to write file after ", max_tries, " attempts.")
  }
}