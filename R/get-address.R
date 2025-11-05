#' Get full URL for file attachments or links on gov.uk
#'
#' @param web_address Webpage to look for the file on
#' @param file_type File extension to search for (e.g. "xlsx", "ods"); default
#' = `NULL`, leave as NULL if searching for a link rather than a file
#' @param file_number If there are multiple files of the same type as
#' `file_type` on the page, which one to return
#' @param search_term Regex; Optional, used by `str_subset` to filter on link URLs
#' @param find_csv_preview Logical, default = `FALSE`; when searching for a CSV
#' on gov.uk, this will return the link to the CSV preview when set to TRUE, and
#' will return the link to the actual CSV when set to FALSE
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_subset
#'
#' @family commentary functions
#'
#' @author
#' Agriculture in the UK team ([AUK_stats_team@defra.gov.uk](mailto:AUK_stats_team@defra.gov.uk))
#'
#' Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export

get_address <- function(web_address, file_type = NULL, file_number = 1,
                        search_term = NULL, find_csv_preview = F) {

  tmp <- tempfile()

  download.file(web_address, destfile = tmp, quiet = TRUE)

  page <- xml2::read_html(tmp)

  files <- page %>%
    # find all links
    rvest::html_nodes("a") %>%
    # get the url
    rvest::html_attr("href") %>%
    # if provided, subset for relevant file types
    { if (!is.null(file_type)) str_subset(., paste0("\\.", tolower(file_type), "$")) else . } %>%
    unique()

  # When searching for a CSV on gov.uk, CSV previews will also be returned
  # This is an issue if you are looking for, say, the second CSV file on the page,
  # but the second CSV link would be the preview of the first CSV file
  # This will either return only the CSV previews or remove the CSV preview links
  if (find_csv_preview) {
    files <- str_subset(files, "csv-preview")
  } else {
    files <- str_subset(files, "csv-preview", negate = T)
  }

  file <- if (!is.null(search_term)) {
    # filter for files with the search term
    str_subset(files, search_term)[[file_number]]
  } else {
    files[[file_number]]
  }

  return(file)

}
