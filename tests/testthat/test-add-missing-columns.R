test_df <- dplyr::tibble(a = 1:5, b = 6:10, d = 11:15, g = 16:20)
all_cols <- letters[1:7]

test_that("columns are added, filled with NA, and reordered correctly by default", {
  expect_equal(add_missing_columns(test_df, all_cols),
               dplyr::tibble(a = 1:5, b = 6:10, c = NA, d = 11:15,
                             e = NA, f = NA, g = 16:20))
})

test_that("when ordering is turned off, columns are not reordered", {
  expect_equal(add_missing_columns(test_df, all_cols, reorder = FALSE),
               dplyr::tibble(a = 1:5, b = 6:10, d = 11:15, g = 16:20,
                             c = NA, e = NA, f = NA))
})

test_that("fill value argument works", {
  expect_equal(add_missing_columns(test_df, all_cols, fill_value = 3),
               dplyr::tibble(a = 1:5, b = 6:10, c = 3, d = 11:15,
                             e = 3, f = 3, g = 16:20))
})
