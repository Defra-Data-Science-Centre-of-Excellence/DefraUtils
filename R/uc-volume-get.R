#' @title Download a file from the DASH Unity Catalog
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description Downloads a file from the Databricks Unity Catalog (UC) to the
#'   RStudio server. This function wraps a call to [httr::GET()] and was adapted
#'   from the DASH playbook.
#'
#'   You must have a valid Databricks Personal Access Token (PAT) set up in your
#'   Databricks account.
#'
#'   For uploading files to UC, see [DefraUtils::uc_volume_put()].
#'
#' @param workspace String. The full URL of the Databricks workspace.
#'
#' @param volume String. The full path to the file in the Unity Catalog (e.g.,
#'   "/Volumes/...").
#'
#' @param token String. Your Databricks Personal Access Token (PAT).
#'
#' @param out_file String. The local file path on the RStudio server where the
#'   file should be saved
#'
#' @return No return value. The file is saved to the specified location on the
#'   RStudio server.
#'
#' @examples
#' \dontrun{
#' uc_volume_get(
#'   workspace = "https://adb-7422054397937474.14.azuredatabricks.net",
#'   volume = "/Volumes/prd_dash_lab/<path-to-file>/filename.csv",
#'   token = "dapid4b3d********************a687f9b",
#'   out_file = here::here("data", "filename.csv")
#' )
#' }
#'
#' @seealso [httr::GET()], [DefraUtils::uc_volume_put()]
#'
#' @export
uc_volume_get <- function(
  workspace,
  volume,
  token,
  out_file
) {
  # set URL
  url <- glue::glue("{workspace}/api/2.0/fs/files/{volume}")

  # set header (with DataBricks PAT)
  headers <- httr::add_headers("Authorization" = glue::glue("Bearer {token}"))


  # make the GET request
  response <- httr::GET(
    url,
    headers,
    httr::write_disk(out_file,
      overwrite = TRUE
    )
  )

  # Extract content from the response
  httr::stop_for_status(response)
}
