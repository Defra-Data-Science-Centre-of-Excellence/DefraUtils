#' Fixes column where numbers are stored as text
#'
#' If you have markers on your values (e.g. 'u' for low reliability, 'p' for
#' provisional, see [AF guidance](https://analysisfunction.civilservice.gov.uk/policy-store/symbols-in-tables-definitions-and-help/#section-6)
#' for full list), you will still get 'numbers stored as text' errors on the
#' unmarked values, even with the latest version of [aftables], which uses [openxlsx2].
#'
#' This function overwrites only cells that can be converted to numbers (i.e. the
#' unmarked values), leaving the marked values and any suppressed values as-is.
#' This function works with both [openxlsx] `Workbook` objects and [openxlsx2]
#' `wbWorkbook` objects.
#'
#' This function is adapted from `rapid.spreadsheets::overwrite_df()`; for the
#' full rapid.spreadsheets code, see
#' [the GitHub page](https://github.com/RAPID-ONS/rapid.spreadsheets/blob/main/R/create_data_table_tab.R).
#'
#' @importFrom openxlsx writeData
#' @importFrom dplyr pull if_else
#' @importFrom stringr str_remove_all str_detect
#'
#' @param excel_wb Openxlsx workbook name
#' @param sheet Worksheet (either name as string, or location as numeric)
#' @param cols Vector of column numbers to be overwritten
#' @param rows Vector of row numbers to be overwritten
#' @param df Data frame containing the data from the relevant worksheet
#' @param num_format String, default = `"0"`; Excel-style cell format for the values
#'
#' @return Updated workbook with modified columns
#'
#' @examples \dontrun{
#' library(aftables)
#'
#' set.seed(1)
#'
#' # Create an aftable
#' cover_df <- list("Section" = c("Title", "Content"))
#'
#' contents_df <- data.frame("Sheet name" = "Table",
#'                           "Sheet title" = "Example",
#'                           check.names = FALSE)
#'
#' dummy_data <- round_with_commas(rnorm(10) * 1e5)
#'
#' dummy_data_markers <- c(dummy_data[1:3],
#'                         paste(dummy_data[4], "[u]"),
#'                         dummy_data[5:10])
#'
#' table_df <- data.frame(
#'   Category = LETTERS[1:10],
#'   "Dummy data" = dummy_data,
#'   "Dummy data with markers" = dummy_data_markers,
#'   check.names = FALSE)
#'
#' aftable <- create_aftable(
#'   tab_titles = c("Cover", "Contents", contents_df$`Sheet name`),
#'   sheet_types = c("cover", "contents", "tables"),
#'   sheet_titles = c("Cover", "Contents", "Table"),
#'   sources = c(rep(NA_character_, 2), "Source"),
#'   tables = list(cover_df, contents_df, table_df))
#'
#' excel_wb <- generate_workbook(aftable)
#'
#' # Check the file
#' # note the format errors on the table sheet
#' openxlsx2::wb_open(excel_wb)
#'
#' # Fix the errors
#' overwrite_num_cols(excel_wb, sheet = 3, cols = 2:3,
#'                    rows = 5:14, df = table_df)
#'
#' # Check the file again
#' openxlsx2::wb_open(excel_wb)
#' }
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export

overwrite_num_cols <- function(excel_wb, sheet, cols, rows, df, num_format = "0") {

  if (class(excel_wb)[1] == "Workbook") {
    wb_class <- "openxlsx"
  } else if (class(excel_wb)[1] == "wbWorkbook") {
    wb_class <- "openxlsx2"
  } else {
    stop("Class of `excel_wb` does not match either openxlsx or openxlsx2")
  }

  lapply(seq_along(cols), \(col) {

    full_col <- pull(df[cols], col)

    # Only convert numbers to numeric if they aren't marked
    full_col_num <- str_remove_all(full_col, "(,|%)(?!.*\\[.*\\])")

    lapply(seq_along(rows), \(row) {

      # If the cell contains a character (e.g. [c]), return the character value
      if (is.na(suppressWarnings(as.numeric(full_col_num[[row]])))) {

        new_value <- full_col_num[[row]]

      } else {

        # If the cell contains a number, return the numeric value (if it's a
        # percentage, divide the value by 100)
        new_value <- if_else(isTRUE(str_detect(full_col[[row]], "%")),
                             as.numeric(full_col_num[[row]]) / 100,
                             as.numeric(full_col_num[[row]]))

      }

      # Write new cell value to workbook and add number formatting
      if (wb_class == "openxlsx") {

        openxlsx::writeData(excel_wb, sheet, new_value,
                            startCol = cols[col],
                            startRow = (row - 1) + rows[1])

        openxlsx::addStyle(excel_wb, sheet,
                           cols = cols[col], rows = (row - 1) + rows[1],
                           openxlsx::createStyle(numFmt = num_format))

      } else if (wb_class == "openxlsx2") {

        excel_wb$add_data(sheet, new_value,
                          start_col = cols[col],
                          start_row = (row - 1) + rows[1])

        excel_wb$add_numfmt(sheet, numfmt = num_format,
                            dims = paste0(LETTERS[cols[col]], (row - 1) + rows[1]))

      }

    })
  })
}
