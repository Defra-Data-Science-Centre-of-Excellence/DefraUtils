#' Generate A Workbook Object From An 'aftable' without an author in the file
#'
#' Populate an 'openxlsx' Workbook-class object with content from an
#' aftable-class object. In turn, the output can be passed to
#' \code{\link[openxlsx]{saveWorkbook}} from 'openxlsx'
#'
#' Have added the `creator = NULL` argument into createWorkbook so that there
#' will be no file author
#'
#' @import aftables
#'
#' @param aftable An aftable-class object created using
#'     \code{\link{create_aftable}} (or \code{\link{as_aftable}}), which
#'     contains the data and information needed to create a workbook.
#'
#' @return A Workbook-class object
#'
#' @export

generate_anon_workbook <- function(aftable) {

  if (!aftables:::is_aftable(aftable)) {
    stop("The object passed to argument 'content' must have class 'aftable'.")
  }

  # Create a table_name from tab_title (unqiue, no spaces, no punctuation)
  aftable[["table_name"]] <-
    gsub(" ", "_", tolower(trimws(aftable[["tab_title"]])))
  aftable[["table_name"]] <-
    gsub("(?!_)[[:punct:]]", "", aftable[["table_name"]], perl = TRUE)

  # Create workbook, add tabs, cover, contents (required for all workbooks)
  wb <- openxlsx::createWorkbook(creator = NULL)
  wb <- aftables:::.add_tabs(wb, aftable)
  wb <- aftables:::.add_cover(wb, aftable)
  wb <- aftables:::.add_contents(wb, aftable)

  # There won't always be a notes tab
  if (any(aftable$sheet_type %in% "notes")) {
    wb <- aftables:::.add_notes(wb, aftable)
  }

  # Iterable titles for tabs containing tables
  table_sheets <- aftable[aftable$sheet_type == "tables", ][["table_name"]]

  for (i in table_sheets) {
    wb <- aftables:::.add_tables(wb, aftable, table_name = i)
  }

  return(wb)

}
