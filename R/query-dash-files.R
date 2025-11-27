#' @title Query files within a DASH directory
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description Query files in a DASH directory (volume or folder) using the
#'   `brickster` API. Designed to handle intermittent http2 errors by retrying
#'   failed attempts. Requires the Defra DASH platform and appropriate
#'   `brickster` environment variables.
#'
#' @details These functions wrap [brickster::db_volume_list()], 
#' [brickster::db_volume_dir_exists()], and [brickster::db_volume_file_exists()] 
#' with a retry loop to handle intermittent http2 errors, which are common but 
#' typically transient. These errors are not usually caused by incorrect code or 
#' file paths, and rerunning the same request often succeeds.
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
#' @return List of files in DASH directory returned, or boolean (for exists fns).
#'
#' @examples
#' \dontrun{
#' # list files in volume/directory
#' create_dash_dir(
#'   path = "/Volumes/prd_dash_lab/<volume-name>/<directory-name>"
#' )
#' 
#' exists_dash_dir(
#'   path = "/Volumes/prd_dash_lab/<volume-name>/<directory-name>"
#' )
#' 
#' exists_dash_file(
#'   path = "/Volumes/prd_dash_lab/<volume-name>/<directory-name>/<file-name>"
#' )
#' }
#'
#' 
#' @seealso [brickster::db_volume_list()], [brickster::db_volume_dir_exists()], 
#' [brickster::db_volume_file_exists()]
#'
#' @name query_dash_files
#'
#' @export
NULL


#' @rdname query_dash_files
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


#' @rdname query_dash_files
#' @export
exists_dash_dir <- function(..., max_tries = 5, interval = 2) {
  attempt <- 1
  
  while (attempt <= max_tries) {
    result <- tryCatch(
      {
        # If this runs without error, the directory exists
        brickster::db_volume_dir_exists(...)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        
        # Case 1: Directory does not exist → return FALSE immediately
        if (grepl("404", msg)) {
          return(FALSE)
        }
        
        # Case 2: Retryable HTTP/2 streaming error
        if (grepl("HTTP/2 stream", msg, ignore.case = TRUE) ||
            grepl("Failed to perform HTTP request", msg)) {
          return(structure("retry", class = "retry"))
        }
        
        # Case 3: Unexpected error → stop
        stop(e)
      }
    )
    
    # If directory exists (TRUE/FALSE), return it
    if (!inherits(result, "retry")) {
      return(result)
    }
    
    # Retry logic
    if (attempt < max_tries) {
      Sys.sleep(interval)
    }
    attempt <- attempt + 1
  }
  
  stop("Failed to check directory after ", max_tries, " attempts.")
}


#' @rdname query_dash_files
#' @export
exists_dash_file <- function(..., max_tries = 5, interval = 2) {
  attempt <- 1
  
  while (attempt <= max_tries) {
    result <- tryCatch(
      {
        # If this runs without error, the file exists
        brickster::db_volume_file_exists(...)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        
        # Case 1: File does not exist → return FALSE immediately
        if (grepl("404", msg)) {
          return(FALSE)
        }
        
        # Case 2: Retryable HTTP/2 streaming error
        if (grepl("HTTP/2 stream", msg, ignore.case = TRUE) ||
            grepl("Failed to perform HTTP request", msg)) {
          return(structure("retry", class = "retry"))
        }
        
        # Case 3: Unexpected error → stop
        stop(e)
      }
    )
    
    # If file exists (TRUE/FALSE), return it
    if (!inherits(result, "retry")) {
      return(result)
    }
    
    # Retry logic
    if (attempt < max_tries) {
      Sys.sleep(interval)
    }
    attempt <- attempt + 1
  }
  
  stop("Failed to check file after ", max_tries, " attempts.")
}

