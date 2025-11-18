test_df <- tibble::tribble(
  ~group_a, ~group_b, ~group_c, ~value, ~sample_size, ~observations,
  "a1",     "b1",     "c1",     16.2,   2,            1,
  "a1",     "b2",     "c2",     5.6,    3,            5,
  "a1",     "b3",     "c2",     2.3,    10,           8,
  "a1",     "b4",     "c2",     7.3,    12,           8,
  "a2",     "b1",     "c2",     9.9,    1,            1,
  "a2",     "b2",     "c3",     1.3,    3,            1,
  "a2",     "b3",     "c3",     4.2,    8,            1,
  "a2",     "b4",     "c3",     5.5,    152,          4
)

test_df_complex <- tibble::tribble(
  ~group_a, ~group_b, ~group_c, ~group_d, ~value, ~sample_size,
  "a1",     "b1",     "c2",     "d",      16.2,   2,
  "a1",     "b2",     "c2",     "d",      5.6,    3,
  "a1",     "b3",     "c2",     "d",      2.3,    10,
  "a1",     "b4",     "c2",     "d",      7.3,    12,
  "a2",     "b1",     "c2",     "d",      9.9,    1,
  "a2",     "b2",     "c2",     "d",      1.3,    3,
  "a2",     "b3",     "c2",     "d",      4.2,    8,
  "a2",     "b4",     "c2",     "d",      5.5,    152,
  "a1",     "b1",     "c2",     "dd",     6.2,    9,
  "a1",     "b2",     "c2",     "dd",     9.6,    11,
  "a1",     "b3",     "c2",     "dd",     3.3,    5,
  "a1",     "b4",     "c2",     "dd",     7.9,    2,
  "a2",     "b1",     "c2",     "dd",     9.9,    1,
  "a2",     "b2",     "c2",     "dd",     21.3,   3,
  "a2",     "b3",     "c2",     "dd",     14.2,   8,
  "a2",     "b4",     "c2",     "dd",     9.5,    110,
  "a1",     "b1",     "c3",     "dd",     5.5,    9,
  "a1",     "b2",     "c3",     "dd",     1.5,    3,
  "a1",     "b3",     "c3",     "dd",     3.3,    2,
  "a1",     "b4",     "c3",     "dd",     7.9,    9,
  "a2",     "b1",     "c3",     "dd",     1.2,    5,
  "a2",     "b2",     "c3",     "dd",     11.3,   9,
  "a2",     "b3",     "c3",     "dd",     22.2,   18,
  "a2",     "b4",     "c3",     "dd",     1.5,    95,
)

test_that("simple (one-level) suppression works as expected", {

  expect_named(fix_suppression(df = test_df, groups = list("group_a")), names(test_df))

  expect_message(fix_suppression(df = test_df, groups = list("group_a")),
                 "Suppression loop 1")
  expect_message(fix_suppression(df = test_df, groups = list("group_a")),
                 "Suppression complete; circular suppression not required")
  expect_no_message(fix_suppression(df = test_df, groups = list("group_a")),
                    message = "Suppression loop 2")
  expect_no_message(fix_suppression(df = test_df, groups = list("group_a")),
                    message = "Circular suppression complete")

  # Fixing by group_a results in suppression
  expect_equal(fix_suppression(df = test_df, groups = list("group_a"))$sample_size,
               c(2, 3, 10, 12, 1, 1, 1, 152))

  # Fixing by group_b should not change anything
  expect_message(fix_suppression(df = test_df, groups = list("group_b")),
                 "Suppression not required")

  expect_no_message(fix_suppression(df = test_df, groups = list("group_b")),
                    message = "Suppression loop 1")
  expect_no_message(fix_suppression(df = test_df, groups = list("group_b")),
                    message = "Suppression complete; circular suppression not required")
  expect_no_message(fix_suppression(df = test_df, groups = list("group_b")),
                    message = "Suppression loop 2")
  expect_no_message(fix_suppression(df = test_df, groups = list("group_b")),
                    message = "Circular suppression complete")

  expect_equal(fix_suppression(df = test_df, groups = list("group_b"))$sample_size,
               test_df$sample_size)

})

test_that("having an orphan level (e.g. only one row for that level) works as expected", {

  expect_equal(fix_suppression(df = test_df,groups = list("group_c"))$sample_size,
               c(2, 1, 1, 12, 1, 1, 1, 152))

  expect_no_message(fix_suppression(df = test_df, groups = list("group_c")),
                    message = "Suppression loop 2")

})

test_that("two-level suppression works as expected", {

  expect_equal(fix_suppression(df = test_df,groups = list("group_a", "group_b"))$sample_size,
               c(2, 3, 1, 12, 1, 1, 1, 152))

  expect_no_message(fix_suppression(df = test_df, groups = list("group_c")),
                    message = "Suppression loop 2")

})

test_that("a custom sample_size column name works as expected", {

  expect_equal(fix_suppression(df = test_df,groups = list("group_a", "group_b"),
                               sample_size_col = "observations")$observations,
               c(1, 1, 1, 1, 1, 1, 1, 1))

  expect_equal(fix_suppression(df = dplyr::rename(test_df, test_sample = sample_size),
                               groups = list("group_a", "group_b"),
                               sample_size_col = "test_sample")$test_sample,
               c(2, 3, 1, 12, 1, 1, 1, 152))

})

test_that("a breakdown that requires circular suppression works as expected", {

  expect_message(suppressWarnings(
    fix_suppression(df = test_df_complex,
                    groups = list(c("group_d", "group_c", "group_b"),
                                  c("group_d", "group_c", "group_a")))),
    "The input groups or data require circular suppression")

  expect_message(suppressWarnings(
    fix_suppression(df = test_df_complex,
                    groups = list(c("group_d", "group_c", "group_b"),
                                  c("group_d", "group_c", "group_a")))),
    "Suppression loop 2")

  expect_message(suppressWarnings(
    fix_suppression(df = test_df_complex,
                    groups = list(c("group_d", "group_c", "group_b"),
                                  c("group_d", "group_c", "group_a")))),
    "Circular suppression complete")

  expect_warning(fix_suppression(df = test_df_complex,
                                 groups = list(c("group_d", "group_c", "group_b"),
                                               c("group_d", "group_c", "group_a"))),
                 "Circular suppression is required, but save_excel_file is FALSE")

  expect_equal(suppressWarnings(
    fix_suppression(df = test_df_complex,
                    groups = list(c("group_d", "group_c", "group_b"),
                                  c("group_d", "group_c", "group_a"))))$sample_size,
    c(2, 3, 1, 12, 1, 1, 1, 152, 1, 1, 5, 1, 1, 1, 8, 1, 9, 1, 1, 9, 5, 1, 1, 95))

})

test_that("creates specified file", {
  save_dir <- tempdir()
  save_file <- file.path(save_dir, "circular-suppression-checks/test_file.xlsx")
  on.exit(unlink(save_file))
  expect_false(file.exists(save_file))
  fix_suppression(df = test_df_complex,
                    groups = list(c("group_d", "group_c", "group_b"),
                                  c("group_d", "group_c", "group_a")),
                  save_excel_file = TRUE, export_path = save_dir, file_name = "test_file")
  expect_true(file.exists(save_file))
})

test_that("creates file when name not specified", {
  save_dir <- tempdir()
  save_file <- file.path(save_dir, "circular-suppression-checks/group_d_group_c_group_b_group_d_group_c_group_a.xlsx")
  on.exit(unlink(save_file))
  expect_false(file.exists(save_file))
  fix_suppression(df = test_df_complex,
                  groups = list(c("group_d", "group_c", "group_b"),
                                c("group_d", "group_c", "group_a")),
                  save_excel_file = TRUE, export_path = save_dir)
  expect_true(file.exists(save_file))
})
