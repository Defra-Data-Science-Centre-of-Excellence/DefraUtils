test_that("expected output is returned", {
  expect_s4_class(get_cell_style(), "Style")
})

style_items <- list(
  altered_by_get_cell_style = c(
    "borderBottom", "borderRight", "borderTop",
    "borderBottomColour", "borderRightColour", "borderTopColour",
    "fontDecoration", "halign", "indent", "numFmt", "wrapText"),
  unaltered_by_get_cell_style_null = c(
    "borderDiagonal", "borderDiagonalColour", "borderLeft", "borderLeftColour",
    "fill", "fontColour", "fontFamily", "fontName", "fontScheme", "fontSize",
    "hidden", "locked", "textRotation"),
  unaltered_by_get_cell_style_false = c("borderDiagonalDown", "borderDiagonalUp"))

default_style <- list(
  halign = "left", valign = "bottom", fontDecoration = as.character(NULL),
  borderTop = NULL, borderLeft = NULL, borderRight = NULL, borderBottom = NULL,
  borderTopColour = NULL, borderLeftColour = NULL, borderRightColour = NULL, borderBottomColour = NULL,
  numFmt = NULL, wrapText = TRUE, indent = 0)

# If this test fails, something has changed in the package and all the tests must be updated
test_that("style items are as expected", {
  expect_true(all(unlist(style_items) %in% names(get_cell_style())))
})

test_style <- function(style_to_test, expected_output) {
  expect_true(all(sapply(names(expected_output), \(x) {
    if (is.null(expected_output[[x]])) {
      # Where expected style is NULL
      is.null(style_to_test[[x]])
    } else if (length(expected_output[[x]]) == 0) {
      # Where expected style is of length 0
      length(style_to_test[[x]]) == 0 &
        class(style_to_test[[x]]) == class(expected_output[[x]])
    } else if (is.list(expected_output[[x]])) {
      # Where expected is a list
      all(unlist(style_to_test[[x]]) == unlist(expected_output[[x]]))
    } else {
      # Everything else
      style_to_test[[x]] == expected_output[[x]]
    }
  })))
  expect_true(all(sapply(style_items$unaltered_by_get_cell_style_null,
                         \(x) is.null(style_to_test[[x]]))))
  expect_false(all(sapply(style_items$unaltered_by_get_cell_style_false,
                          \(x) style_to_test[[x]])))
}

test_that("default output is as expected", {
  test_style(as.list(get_cell_style()), default_style)
})

test_that("number output is as expected", {
  # Get all the default values and only overwrite the changed ones
  number_body_style <- lapply(names(default_style), \(x) {
    if (x == "numFmt") {
      list(
        numFmtId = 165,
        formatCode = "[&lt;-9.99]-#,##0;[&gt;9.99]#,##0;#0.0"
      )
    } else if (x == "halign") {
      "right"
    } else if (x == "wrapText") {
      NULL
    } else {
      default_style[[x]]
    }
  }) %>% set_names(names(default_style))
  test_style(as.list(get_cell_style(style1 = "number")), number_body_style)
})

test_that("percent output is as expected", {
  # Get all the default values and only overwrite the changed ones
  percent_body_style <- lapply(names(default_style), \(x) {
    if (x == "numFmt") {
      list(
        numFmtId = 165,
        formatCode = "##0%"
      )
    } else if (x == "halign") {
      "right"
    } else if (x == "wrapText") {
      NULL
    } else {
      default_style[[x]]
    }
  }) %>% set_names(names(default_style))
  test_style(as.list(get_cell_style(style1 = "percent")), percent_body_style)
})

test_that("custom output is as expected", {
  # Get all the default values and only overwrite the changed ones
  custom_body_style <- lapply(names(default_style), \(x) {
    if (x == "numFmt") {
      list(
        numFmtId = 165,
        formatCode = "£#.#"
      )
    } else if (x == "halign") {
      "right"
    } else if (x == "wrapText") {
      NULL
    } else {
      default_style[[x]]
    }
  }) %>% set_names(names(default_style))
  test_style(as.list(get_cell_style(style1 = "custom", custom_format = "£#.#")),
             custom_body_style)
})

test_that("heading output is as expected", {
  # Get all the default values and only overwrite the changed ones
  text_heading_style <- c(
    lapply(names(default_style), \(x) {
      if (x == "fontDecoration") {
        "BOLD"
      } else if (x == "valign") {
        "center"
      } else if (x %in% c("borderTop", "borderBottom")) {
        "thin"
      } else if (x %in% c("borderTopColour", "borderBottomColour")) {
        "FF000000"
      } else {
        default_style[[x]]
      }
    }) %>% set_names(names(default_style)),
    list())
  test_style(as.list(get_cell_style(style2 = "heading")), text_heading_style)
})

test_that("highlight output is as expected", {
  # Get all the default values and only overwrite the changed ones
  text_highlight_style <- c(
    lapply(names(default_style), \(x) {
      if (x %in% c("borderTop", "borderBottom")) {
        "thin"
      } else if (x %in% c("borderTopColour", "borderBottomColour")) {
        "FF000000"
      } else {
        default_style[[x]]
      }
    }) %>% set_names(names(default_style)),
    list())
  test_style(as.list(get_cell_style(style2 = "highlight")), text_highlight_style)
})

test_that("lastrow output is as expected", {
  # Get all the default values and only overwrite the changed ones
  text_lastrow_style <- c(
    lapply(names(default_style), \(x) {
      if (x == "borderBottom") {
        "thin"
      } else if (x == "borderBottomColour") {
        "FF000000"
      } else {
        default_style[[x]]
      }
    }) %>% set_names(names(default_style)),
    list())
  test_style(as.list(get_cell_style(style2 = "lastrow")), text_lastrow_style)
})

test_that("bold output is as expected", {
  # Get all the default values and only overwrite the changed ones
  text_body_bold_style <- c(
    lapply(names(default_style), \(x) {
      if (x == "fontDecoration") {
        "BOLD"
      } else {
        default_style[[x]]
      }
    }) %>% set_names(names(default_style)),
    list())
  test_style(as.list(get_cell_style(bold = TRUE)), text_body_bold_style)
})

test_that("separator output is as expected", {
  # Get all the default values and only overwrite the changed ones
  text_body_separator_style <- c(
    lapply(names(default_style), \(x) {
      if (x == "borderRight") {
        "thin"
      } else if (x == "borderRightColour") {
        "FF000000"
      } else {
        default_style[[x]]
      }
    }) %>% set_names(names(default_style)),
    list())
  test_style(as.list(get_cell_style(separator = TRUE)), text_body_separator_style)
})

test_that("indent output is as expected", {
  # Get all the default values and only overwrite the changed ones
  text_body_indent_style <- c(
    lapply(names(default_style), \(x) {
      if (x == "indent") {
        3
      } else {
        default_style[[x]]
      }
    }) %>% set_names(names(default_style)),
    list())
  test_style(as.list(get_cell_style(indent = 3)), text_body_indent_style)
})
