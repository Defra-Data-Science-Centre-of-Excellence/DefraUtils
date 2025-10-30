#' @title Read data of various file types from the DASH data lake
#'
#' @author Josh Moatt
#'
#' @description A set of convenience functions to read data from common file
#'   formats from the DASH platform. Each function uses `brickster` and wraps
#'   the appropriate reader function for each file type. These functions will
#'   avoid the relatively frequent http2 errors that crop up with `brickster`.
#'   See details for more info on how they solve this problem.  In order for
#'   these functions to work, you must be working on the Defra DASH platform and
#'   have set the required brickster environmental variables. There a specific
#'   functions for reading in .csv, .xlsx, and .Rds files as well as a generic
#'   function for reading in additional file types.
#'
#' @details These functions are designed to handle the frequent http2 errors
#'   that occur with `brickster`. From testing, these errors are not code or
#'   file path errors, but are just minor bugs with the API. Often, if the same
#'   code is rerun, the data will be read in fine.
#'
#'   There is one generic function which can be used to create a temporary file
#'   for any file type (using the ext argument). There are also wrapper
#'   functions for specifically reading in .csv, .Rds, and .xlsx files.
#'
#'   All of the functions work in the same way. They use
#'   [brickster::db_volume_read()], to attempt to read in the data and store it
#'   in an appropriate temporary file. They use a retry loop that catch errors
#'   with [tryCatch()]. If no error is thrown by `brickster`, the functions read
#'   in the temporary file into R using an appropriate reader function and
#'   returns the resulting data frame. If an error is thrown by `brickster`, the
#'   functions will wait a set number of seconds (controlled by the interval
#'   argument) before retrying. They will repeat the attempted data load until
#'   either the file is read successfully or the maximum number of attempts (set
#'   by max_tries) is reached - after which it throws an error.
#'
#'   In order for these functions to work, you must be working on the Defra DASH
#'   platform and have set the required `brickster` environmental variables.
#'   These variables include setting a databricks Personal Access Token (PAT).
#'   For more information on how to do this, see the specific project README.
#'
#'   Reader functions used are as follows:
#'
#'   \itemize{
#'     \item **.csv** - [readr::read_csv()]
#'     \item **.xlsx** - [readxl::read_xlsx()]
#'     \item **.Rds** - [readr::read_rds()]
#'   }
#'
#' @param path A string containing the path to data to be read in. Should be the
#'   full DASH string starting "/Volumes/..."
#'
#' @param ext A sting specifying the file type. Only used in the general
#'   function. for the specific functions, this will be set to the correct
#'   extension (e.g. .csv).
#'
#' @param ... Additional arguments passed to the appropriate reader function.
#'
#' @param max_tries Maximum number of tries to read in data. Default is 5.
#'
#' @param interval Interval between tries to read in data. Default is 2 seconds.
#'
#' @return a dataframe
#'
#' @examples
#' \dontrun{
#' # read Rds file
#' read_rds_from_volume(
#'   path = "/Volumes/prd_dash_lab/<path-to-file>/filename.Rds"
#' )
#'
#' # read csv file
#' read_csv_from_volume(
#'   path = "/Volumes/prd_dash_lab/<path-to-file>/filename.csv",
#'   show_col_types = FALSE
#' )
#'
#' # read xlsx file
#' read_xlsx_from_volume(
#'   path = "/Volumes/prd_dash_lab/<path-to-file>/filename.xlsx",
#'   sheet = "sheet-name"
#' )
#' }
#'
#' @seealso [brickster::db_volume_read()], [readr::read_csv()], [readr::read_rds()], [readxl::read_xlsx()]
#'
#' @name read_files_from_volume
#'
#' @export
NULL

#' @rdname read_files_from_volume
#' @export
read_file_from_volume <- function(path, ext, max_tries = 5, interval = 2) {
  attempt <- 1
  success <- FALSE
  tmp <- NULL

  while (attempt <= max_tries && !success) {
    tryCatch(
      {
        tmp <- brickster::db_volume_read(
          path = path,
          destination = tempfile(fileext = glue::glue("{ext}"))
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
    stop("Failed to read volume after ", max_tries, " attempts.")
  }

  result <- tmp

  return(result)
}

#' @rdname read_files_from_volume
#' @export
read_csv_from_volume <- function(path, ..., max_tries = 5, interval = 2) {
  # read in data using general function set to Rds
  tmp <- DefraUtils::read_file_from_volume(
    path = path,
    ext = ".csv",
    max_tries = max_tries,
    interval = interval
  )

  result <- readr::read_csv(tmp, ...)

  return(result)
}

#' @rdname read_files_from_volume
#' @export
read_rds_from_volume <- function(path, ..., max_tries = 5, interval = 2) {
  # read in data using general function set to Rds
  tmp <- DefraUtils::read_file_from_volume(
    path = path,
    ext = ".Rds",
    max_tries = max_tries,
    interval = interval
  )

  # This will throw an error if tmp is invalid, and print the message
  result <- readr::read_rds(tmp, ...)

  return(result)
}

#' @rdname read_files_from_volume
#' @export
read_xlsx_from_volume <- function(path, ..., max_tries = 5, interval = 2) {
  # read in data using general function set to Rds
  tmp <- DefraUtils::read_file_from_volume(
    path = path,
    ext = ".xlsx",
    max_tries = max_tries,
    interval = interval
  )

  result <- readxl::read_xlsx(tmp, ...)

  return(result)
}
