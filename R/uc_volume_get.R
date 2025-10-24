#' @title Copy file from the DASH Unity Catalog
#'
#' @author Josh Moatt
#'
#' @description This is a simple function to read data from the databricks
#'   Unuity Catalog (UC) into RStudio. It is built using the [GET] function from
#'   the httr package. This function was lifted directly from the DASH playbook.
#'   For saving files, see `uc_volume_put`.
#'
#'   Note, you must have set up a databricks Personal Access Token (PAT) in the
#'   databricks settings before doing this.
#'
#' @param workspace databricks workspace (string)
#'
#' @param volume file path to data folder (string)
#'
#' @param token databricks PAT (string)
#'
#' @param out_file filepath on RStudio Cluster that data should be stored.
#'
#' @return data saved in Rstudio cluster.
#'
#' @examples
#' \dontrun{
#' uc_volume_get(
#'   workspace = "https://adb-7422054397937474.14.azuredatabricks.net",
#'   volume = "/Volumes/prd_dash_lab/<path-to-file>/filename.csv",
#'   token = "dapid4b3d********************a687f9b",
#'   out_file = here::here("data", "filename.csv")
#' )
#'}
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