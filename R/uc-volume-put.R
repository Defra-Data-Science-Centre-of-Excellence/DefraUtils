#' @title Write file to the DASH Unity Catalog
#'
#' @author Josh Moatt
#'
#' @description This is a simple function to save a file to the DASH Unity
#'   Catalog (UC). It is built using the httr2 package and uses a PUT request to
#'   save the file. It will save a local file, either from a drive or temp
#'   directory. For reading data, see `uc_volume_get`.
#'
#'   Note, you must have set up a databricks Personal Access Token (PAT) in the
#'   databricks settings before doing this.
#'
#' @param workspace databricks workspace (string)
#'
#' @param volume path to folder on databricks where data should be written
#'   (string)
#'
#' @param token databricks PAT (string)
#'
#' @param file name of file to be exported including file extension. Will be
#'   used to both specify the file to export and specify the file to be created
#'   on databricks (String).
#'
#' @param folder path to file you wish to save, excluding file name and
#'   extension (string).
#'
#' @return file saved on DASH.
#'
#' @examples
#' \dontrun{
#' uc_volume_put(
#'   workspace = "https://adb-7422054397937474.14.azuredatabricks.net",
#'   volume = "/Volumes/prd_dash_lab/<path-to-file>",
#'   token = "dapid4b3d********************a687f9b",
#'   file = "filename.csv",
#'   folder = here::here("data")
#' )
#' }
#'
#' @export
uc_volume_put <- function(
  workspace,
  volume,
  token,
  file,
  folder
) {
  # set URL
  url <- glue::glue("{workspace}/api/2.0/fs/files/{volume}/{file}")

  # combine folder and file to get out_file
  out_file <- glue::glue("{folder}/{file}")

  # write data to data bricks
  response <- httr2::request(url) %>%
    httr2::req_method("PUT") %>% # PUT request seems to work best
    httr2::req_headers(
      Authorization = glue::glue("Bearer {token}"), # use data bricks PAT
      `Content-Type` = "multipart/form-data"
    ) %>%
    httr2::req_body_file(out_file) %>% # specifies file to export
    httr2::req_perform()
}
