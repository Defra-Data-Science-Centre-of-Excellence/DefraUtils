set.seed(1)

# Create an aftable
cover_df <- list("Section" = c("Title", "Content"))

contents_df <- data.frame("Sheet name" = "Table",
                          "Sheet title" = "Example",
                          check.names = FALSE)

table_df <- data.frame(
  Category = LETTERS[1:10],
  "Suppressed" = c(1:4, "[c]", 6:9, "[x]"),
  "Commas" = round_with_commas(rnorm(10) * 1e5, "optimise"),
  check.names = FALSE
)

aftable <- aftables::create_aftable(
  tab_titles = c("Cover", "Contents", contents_df$`Sheet name`),
  sheet_types = c("cover", "contents", "tables"),
  sheet_titles = c("Cover", "Contents", "Table"),
  sources = c(rep(NA_character_, 2), "Source"),
  tables = list(cover_df, contents_df, table_df))

excel_wb <- aftables::generate_workbook(aftable)

test_that("not using overwrite_num_cols results in numbers stored as text", {
  excel_path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(excel_wb, file = excel_path, overwrite = TRUE)
  table_sheet <- tidyxl::xlsx_cells(excel_path, sheets = "Table")
  expect_equal(
    unique(table_sheet$data_type[table_sheet$col %in% 2:3 & table_sheet$row %in% 5:14]),
    "character"
  )
})

test_that("overwrite_num_cols fixes numbers stored as text", {
  overwrite_num_cols(excel_wb, sheet = 3, cols = 2:3, rows = 5:14, df = table_df)
  excel_path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(excel_wb, file = excel_path, overwrite = TRUE)
  table_sheet <- tidyxl::xlsx_cells(excel_path, sheets = "Table")
  expect_equal(
    unique(table_sheet$data_type[table_sheet$col %in% 2:3 & table_sheet$row %in% 5:14]),
    c("numeric", "character")
  )
})
