#' Get cell styles for stats publications
#'
#' @importFrom dplyr if_else
#' @importFrom openxlsx createStyle
#' @importFrom rlang arg_match
#'
#' @param style1 Whether the data is text, numbers or percentages. Text cells
#' get left-aligned, while number and percentage cells get right-aligned. By
#' default, both percentages and numbers are rounded to a whole number; to round
#' to a number of decimal places, define `style1` to `"custom"` and specify the
#' `custom_format` argument.
#' @param style2 Where the data is in the table (heading row, body, average row
#' or last row). Heading cells are bold, aligned vertically in the middle, and
#' get upper and lower borders. All other cells are bottom-aligned. 'Highlight'
#' cells get upper and lower borders, while 'last row' cells just get lower borders.
#' @param bold logical, default = `FALSE`; should the text be bold?
#' @param separator logical, default = `FALSE`; should there be a separating
#' border to the right of the column?
#' @param custom_format Used by numFmt argument of [createStyle()]; an Excel
#' custom number format, e.g., "##0.0%" rounds a percentage to 1 decimal place.
#' @param indent Numeric, default = 0; Horizontal indentation of cell contents.
#'
#' @export

get_cell_style <- function(style1 = c("text", "number", "percent", "custom"),
                           style2 = c("body", "heading", "highlight", "lastrow"),
                           bold = FALSE, separator = FALSE, custom_format = NULL, indent = 0) {

  style1 <- arg_match(style1)
  style2 <- arg_match(style2)

  horizontal_align <- if_else(style1 == "text", "left", "right")

  vertical_align <- if_else(style2 == "heading", "center", "bottom")

  text_decoration <- if (style2 == "heading" | bold) { "bold" } else { NULL }

  border_type <- if (style2 %in% c("heading", "highlight")) { "TopBottom"
  } else if (style2 == "lastrow") { "bottom" } else { NULL }

  if (separator) { border_type <- c(border_type, "right") }

  num_format <- switch(style1,
                       text = "GENERAL",
                       # show numbers greater than 9.99 or less than -9.99 with
                       # comma separators and no decimal places; show numbers
                       # less than 9.99 with 1dp
                       number = "[<-9.99]-#,##0;[>9.99]#,##0;#0.0",
                       percent = "##0%",
                       custom = custom_format)

  wrap_text <- style1 == "text" | style2 == "heading"

  style <- createStyle(
    halign = horizontal_align, valign = vertical_align,
    textDecoration = text_decoration, border = border_type,
    numFmt = num_format, wrapText = wrap_text, indent = indent)

  return(style)

}

