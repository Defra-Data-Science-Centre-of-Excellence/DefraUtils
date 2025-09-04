#' Fixes column where numbers are stored as text
#'
#' Overwrites selected columns and rows that contain both numeric and character
#' elements. Once this is run, you should add a new style to the modified
#' columns and rows so that they are displayed correctly in the output file. Use
#' [openxlsx::addStyle()] and [get_cell_style()] to do this.
#'
#' This function is adapted from `rapid.spreadsheets::overwrite_df()`, to work
#' with [aftables]; for the full rapid.spreadsheets code, see
#' [https://github.com/RAPID-ONS/rapid.spreadsheets/blob/main/R/create_data_table_tab.R].
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
#'
#' @return Updated workbook with modified columns
#'
#' @examples \dontrun{
#' library(openxlsx)
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
#' table_df <- data.frame(
#'   Category = LETTERS[1:10],
#'   "Suppressed" = c(1:4, "[c]", 6:9, "[x]"),
#'   "Commas" = round_with_commas(rnorm(10) * 1e5, "optimise"),
#'   check.names = FALSE
#' )
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
#' openXL(excel_wb)
#'
#' # Fix the errors
#' overwrite_num_cols(excel_wb, sheet = 3, cols = 2:3,
#'                    rows = 5:14, df = table_df)
#'
#' # Check the file again
#' # the errors are gone, but the commas have disappeared
#' openXL(excel_wb)
#'
#' # Add styling
#' addStyle(excel_wb, sheet = 3, cols = 2:3,
#'          rows = 5:14, gridExpand = TRUE,
#'          style = get_cell_style("number", "body"))
#'
#' # Check the file a final time
#' # formatting is back and errors are still gone
#' openXL(excel_wb)
#' }
#'
#' @export

overwrite_num_cols <- function(excel_wb, sheet, cols, rows, df) {

  lapply(seq_along(cols), \(col) {

    full_col <- pull(df[cols], col)
    # Only convert numbers to numeric if they aren't marked with [u]
    full_col_num <- str_remove_all(full_col, "(,|%)(?!.*[u])")

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

      openxlsx::writeData(excel_wb, sheet, new_value,
                          startCol = cols[col],
                          startRow = (row - 1) + rows[1])

    })
  })
}
