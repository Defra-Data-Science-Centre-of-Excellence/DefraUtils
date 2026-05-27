# Function to download partitioned parquet files

#' @title Download partitioned parquet files from a DASH volume
#' 
#' @author James Duffy ([James.Duffy@defra.gov.uk](mailto:James.Duffy@defra.gov.uk))
#' 
#' @description Function to ...
#' 
#' @details This function ...
#'
#' @param path A string containing the directory path on DASH containing the 
#'   target parquet. It should be the full DASH string starting "/Volumes/..."
#' 
#' @param tempdir_path A string containing the temporary directory where parquet 
#'   files will be downloaded. Default is file.path(tempdir(), "tmp_parquet").
#' 
#' @return Invisibly returns `NULL`. Called for side effects (downloads files).
#' 
#' @examples
#'  \dontrun{
#' download_parquet_files(
#'   path = "/Volumes/prd_dash_lab/<volume-name>/<parquet-location>"
#' )
#' }
#'
#' @export
download_parquet_files <- function(
    path,
    tempdir_path = file.path(tempdir(), "tmp_parquet"),
) {
  
  # List volume contents
  cont <- brickster::db_volume_list(path)
  
  # Reset and recreate temp directory
  if (dir.exists(tempdir_path)) {
    unlink(tempdir_path, recursive = TRUE)
  }
  dir.create(tempdir_path, recursive = TRUE)
  
  # Filter parquet files
  parquet_files <- purrr::keep(
    cont$contents,
    ~ grepl("\\.parquet$", .x$name)
  )
  
  # Download parquet files
  purrr::map(
    parquet_files,
    ~ brickster::db_volume_read(
      path = .x$path,
      destination = file.path(tempdir_path, .x$name),
      perform_request = TRUE
    )
  )
  
  message("Parquet files in ", tempdir_path)
  invisible(NULL)
}