#' @title List files within a DASH directory
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description Lists files in a DASH directory (volume or folder) using the
#'   `brickster` API. Designed to handle intermittent http2 errors by retrying
#'   failed attempts. Requires the Defra DASH platform and appropriate
#'   `brickster` environment variables.
#'
#' @details This function wraps [brickster::db_volume_list()] with a retry loop
#' to handle intermittent http2 errors, which are common but typically
#' transient. These errors are not usually caused by incorrect code or file
#' paths, and rerunning the same request often succeeds.
#'
#' The function will retry the request up to \code{max_tries} times, waiting
#' \code{interval} seconds between each attempt. If all attempts fail, an error
#' is thrown.
#'
#' You must be working on the Defra DASH platform and have set the required
#' `brickster` environment variables, including a valid Databricks Personal
#' Access Token (PAT).
#'
#' @param path A string containing the path to the volume or folder. Should be
#'   the full DASH string starting "/Volumes/..."
#'
#' @param max_tries Maximum number of tries to access directory metadata.
#'   Default is 5 attempts.
#'
#' @param interval Interval between tries to read in data. Default is 2 seconds.
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
#' @seealso [brickster::db_volume_list()]
#'
#' @export
list_dash_files <- function(path, max_tries = 5, interval = 2) {
  attempt <- 1
  success <- FALSE
  tmp <- NULL

  while (attempt <= max_tries && !success) {
    tryCatch(
      {
        tmp <- brickster::db_volume_list(
          path = path,
        )
        success <- TRUE
      },
      error = function(e) {
        Sys.sleep(interval)
        attempt <<- attempt + 1
      }
    )
  }

  if (!success) {
    stop("Failed to pull list after ", max_tries, " attempts.")
  }

  return(tmp)
}
