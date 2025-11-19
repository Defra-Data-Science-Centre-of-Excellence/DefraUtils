#' @title Write file to the DASH Unity Catalog
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description Uploads a local file to the DASH Unity Catalog (UC) using a PUT
#'   request via the `httr2` package. This function is useful for saving files
#'   from the RStudio server to Databricks volumes.
#'
#'   You must have a valid Databricks Personal Access Token (PAT) configured in
#'   your Databricks account.
#'
#'   For downloading files from UC, see [DefraUtils::uc_volume_get()].
#'
#' @param workspace String. The full URL of the Databricks workspace.
#'
#' @param volume String. The path to the Databricks volume or folder where the
#'   file should be saved.
#'
#' @param token String. Your Databricks Personal Access Token (PAT).
#'
#' @param file String. The name of the file to upload, including its extension.
#'
#' @param folder String. The local path to the folder containing the file to
#'   upload (excluding the file name).
#'
#' @return No return value. The file is uploaded to the specified location in
#'   the Unity Catalog.
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
#' @seealso [httr2::request()], [DefraUtils::uc_volume_get()]
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
