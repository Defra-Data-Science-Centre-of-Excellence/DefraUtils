#' @title Lidt files within a DASH volume or folder
#'
#' @author Josh Moatt
#'
#' @description Function to streamline listing files in DASH volumes/folders.
#'   This function will help avoid the http2 error which seems to be a frequent
#'   problem with `brickster`. In order for this function to work, you must be
#'   working on the Defra DASH platform and have set the required `brickster`
#'   environmental variables.
#'
#' @details This function is designed to handle the frequent http2 errors that
#'   occur with `brickster`. From testing, these errors are not code or file
#'   path errors, but are just minor bugs with the API. Often, if the same code
#'   is rerun, the data will be read in fine.
#'
#'   This function uses the `brickster::db_volume_list` function. It uses a
#'   retry loop that catch errors with `tryCatch()`. If no error is thrown by
#'   `brickster`, the function will return a list of the files in the specified
#'   DASH directory. If an error is thrown by `brickster`, the function will
#'   wait a set number of seconds (controlled by the interval argument) before
#'   retrying. It will repeat the attempt access the information on the
#'   directory until either the list is created or the maximum number of
#'   attempts (set by max_tries) is reached - after which it throws an error.
#'
#'   In order for this function to work, you must be working on the Defra DASH
#'   platform and have set the required brickster environmental variables. These
#'   variables include setting a databricks Personal Access Token (PAT).
#'
#' @param path A string containing the path to the volume or folder. Should be
#'   the full DASH string starting "/Volumes/..."
#'
#' @param max_tries Maximum number of tries to read in data. Added to deal with
#'   persistent http errors.
#'
#' @param interval Interval between tries to read in data. Added to deal with
#'   persistent http errors.
#'
#' @return List of files in DASH directory returned.
#'
#' @examples
#' \dontrun{
#' # list files in volume/directory
#' create_dash_dir(
#'   path = "/Volumes/prd_dash_lab/<volume-name>/<directory-name>"
#' )
#' }
#'
#' @export
list_dash_files <- function(path, max_tries = 5, interval = 2) {
  attempt <- 1
  success <- FALSE
  tmp <- NULL
  
  while (attempt <= max_tries && !success) {
    tryCatch({
      tmp <- brickster::db_volume_list(
        path = path,
      )
      success <- TRUE
    }, error = function(e) {
      Sys.sleep(interval)
      attempt <<- attempt + 1
    })
  }
  
  if (!success) {
    stop("Failed to pull list after ", max_tries, " attempts.")
  }
  
  return(tmp)
}