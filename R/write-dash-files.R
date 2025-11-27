#' @title Write files to the DASH Unity Catalog
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description Functions to streamline writing files to DASH volumes. These
#'   functions will help avoid the http2 error which seems to be a frequent
#'   problem with `brickster`. This suite of functions are built on
#'   [brickster::db_volume_write()]. There is a general function,
#'   [DefraUtils::dash_volume_write()] which can be used to save previously
#'   created files to the DASH platform. However, there are also more specific
#'   functions, to save data frames a specific file types (.Rds, .csv, .xlsx
#'   workbooks). These functions use the general function alongside an
#'   appropriate write function, using a temporary file to avoid saving anything
#'   locally. In order for these functions to work, you must be working on the
#'   Defra DASH platform and have set the required `brickster` environmental
#'   variables.
#'
#' @details These functions are designed to handle the frequent http2 errors
#'   that occur with `brickster`. From testing, these errors are not code or
#'   file path errors, but are just minor bugs with the API. Often, if the same
#'   code is rerun, the data will be saved without issue.
#'
#'   There is one generic function which can be used to write existing files to
#'   DASH, covering any file type. There are also wrapper functions for
#'   specifically writing .csv, .Rds, and .xlsx files.
#'
#'   All of the functions  use the general [DefraUtils::dash_volume_write()]
#'   function. This uses [brickster::db_volume_write()], to attempt to write the
#'   data to the specified DASH volume. It uses a retry loop that catches errors
#'   with [tryCatch()]. If no error is thrown by `brickster`, the function will
#'   end as the data has been written. If an error is thrown by `brickster`, the
#'   function will wait a set number of seconds (controlled by the interval
#'   argument) before retrying. It will repeat the attempt to save the data
#'   until either the file is written successfully or the maximum number of
#'   attempts (set by max_tries) is reached - after which it throws an error.
#'
#'   The more specific functions will save objects from your R environment
#'   (usually data frames) to DASH directly, without the need to save them
#'   locally first. This helps avoiding saving data to the project folder, thus
#'   reducing the risk of accidentally committing the data or output to GitHub.
#'   To do this, the functions use an appropriate write function (see below) to
#'   create a temporary file of the right format (e.g. .csv). This file is then
#'   passed to the general function [DefraUtils::dash_volume_write()] and the
#'   file is written to DASH as described above.
#'
#'   Reader functions used are as follows:
#'
#'   \itemize{
#'     \item **.csv** - [readr::write_csv()]
#'     \item **.xlsx** - [openxlsx::saveWorkbook()]
#'     \item **.Rds** - [base::saveRDS()]
#'   }
#'
#'   In order for these functions to work, you must be working on the Defra DASH
#'   platform and have set the required brickster environmental variables. These
#'   variables include setting a databricks Personal Access Token (PAT). For
#'   more information on how to do this, see the specific project README.
#'
#' @param path A string containing the path on DASH to save the file to. Should
#'   be the full DASH string starting "/Volumes/..." including file name and
#'   extension.
#'
#' @param data (All functions except [dash_volume_write()]) Object to be saved.
#'   Usually this will be a data frame or similar. For [write_xlsx_to_volume()],
#'   this must be a workbook object created using the `openxlsx` package.
#'
#' @param file ([dash_volume_write()] only) A string containing the local file
#'   path to the file you want to save to DASH. Should be the full file path
#'   including file name and extension.
#'
#' @param ... Additional arguments passed to [brickster::db_volume_write()].
#'
#' @param max_tries Maximum number of tries to read in data. Added to deal with
#'   persistent http errors. Default is 5.
#'
#' @param interval Interval between tries to read in data. Added to deal with
#'   persistent http errors. Default is 2
#'
#' @return Data file saved to DASH volume.
#'
#' @examples
#' \dontrun{
#' # write existing file
#' dash_volume_write(
#'   path = "/Volumes/prd_dash_lab/<path-to-file>/filename.docx",
#'   file = here::here("outputs", "filename.docx")
#' )
#'
#' # write csv file
#' write_csv_to_volume(
#'   path = "/Volumes/prd_dash_lab/<path-to-file>/filename.csv",
#'   data = my_data_frame
#' )
#'
#' # write xlsx file
#' write_xlsx_to_volume(
#'   path = "/Volumes/prd_dash_lab/<path-to-file>/filename.xlsx",
#'   data = my_data_frame
#' )
#'
#' # write Rds file
#' write_rds_to_volume(
#'   path = "/Volumes/prd_dash_lab/<path-to-file>/filename.Rds",
#'   data = my_data_frame
#' )
#' }
#'
#' @seealso [brickster::db_volume_write()], [readr::write_csv()],
#'   [openxlsx::saveWorkbook()], [base::saveRDS()],
#'   [DefraUtils::write_xlsx_to_volume()], [DefraUtils::write_csv_to_volume()],
#'   [DefraUtils::write_rds_to_volume()]
#'
#' @name write_files_to_volume
#'
#' @export
dash_volume_write <- function(..., max_tries = 5, interval = 2) {
  attempt <- 1
  success <- FALSE

  while (attempt <= max_tries && !success) {
    tryCatch(
      {
        brickster::db_volume_write(...)
        success <- TRUE
      },
      error = function(e) {
        Sys.sleep(interval)
        attempt <<- attempt + 1
      }
    )
  }

  if (!success) {
    stop("Failed to write file after ", max_tries, " attempts.")
  }
}

#' @rdname write_files_to_volume
#' @export
write_xlsx_to_volume <- function(data, path, ...) {
  # Create a temporary .xlsx file
  temp <- tempfile(fileext = ".xlsx")

  # Save the workbook to the temporary file
  openxlsx::saveWorkbook(data, file = temp)

  # Write the file to Brickster volume
  DefraUtils::dash_volume_write(
    path = path,
    file = temp,
    ...
  )
}

#' @rdname write_files_to_volume
#' @export
write_rds_to_volume <- function(data, path, ...) {
  # Create a temporary .xlsx file
  temp <- tempfile(fileext = ".rds")

  # Save the workbook to the temporary file
  saveRDS(data, file = temp)

  # Write the file to Brickster volume
  DefraUtils::dash_volume_write(
    path = path,
    file = temp,
    ...
  )
}

#' @rdname write_files_to_volume
#' @export
write_csv_to_volume <- function(data, path, ...) {
  # Create a temporary .xlsx file
  temp <- tempfile(fileext = ".csv")

  # Save the workbook to the temporary file
  readr::write_csv(
    data,
    file = temp
  )

  # Write the file to Brickster volume
  DefraUtils::dash_volume_write(
    path = path,
    file = temp,
    ...
  )
}

#' @rdname write_files_to_volume
#' @export
write_text_to_volume <- function(data, path, ...) {
  
  # Create a temporary file
  ext <- paste0(".", tools::file_ext(path))
  if(!(ext %in% c(".txt", ".md", ".svg"))) {
    cli::cli_abort("Function only supports .txt, .md and .svg filetypes.")
  }
  
  temp <- tempfile(fileext = ext)
  
  # Save the text file to the temporary file
  cat(
    data,
    file = temp,
    sep = "\n"
  )
  
  
  # Write the file to Brickster volume
  DefraUtils::dash_volume_write(
    path = path,
    file = temp,
    ...
  )
}
