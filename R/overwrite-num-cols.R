#' Fixes column where numbers are stored as text
#'
#' Overwrites selected columns and rows that contain both numeric and character
#' elements. Once this is run, you should add a new style to the modified
#' columns and rows so that they are displayed correctly in the output file. Use
#' [openxlsx::addStyle()] and [get_cell_style()] to do this.
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
