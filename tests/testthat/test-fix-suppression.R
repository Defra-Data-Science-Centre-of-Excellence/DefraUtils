
test_df <- tibble::tribble(
  ~"group_a", ~"group_b", ~"group_c", ~"value", ~"sample_size", ~"nobs",
  "a",         "b",          "c",       16.2,       2,            1,   
  "a",         "bb",         "cc",       5.6,       3,            5, 
  "a",         "bbb",        "cc",       2.3,       10,           8, 
  "a",         "bbbb",       "cc",       7.3,       12,           8,
  "aa",        "b",          "cc",       9.9,       1,            1,
  "aa",        "bb",         "ccc",      1.3,       3,            1,
  "aa",        "bbb",        "ccc",      4.2,       8,            1, 
  "aa",        "bbbb",       "ccc",      5.5,       152,          4
  )

test_df_complex <- tibble::tribble(
  ~"group_a", ~"group_b", ~"group_c", ~"group_d",~"value", ~"sample_size",
  "a",         "b",          "cc",       "d",      16.2,      2,          
  "a",         "bb",         "cc",       "d",      5.6,       3,         
  "a",         "bbb",        "cc",       "d",      2.3,       10,        
  "a",         "bbbb",       "cc",       "d",      7.3,       12,        
  "aa",        "b",          "cc",       "d",      9.9,       1,         
  "aa",        "bb",         "cc",       "d",      1.3,       3,          
  "aa",        "bbb",        "cc",       "d",      4.2,       8,         
  "aa",        "bbbb",       "cc",       "d",      5.5,       152,       
  "a",         "b",          "cc",       "dd",     6.2,       9,          
  "a",         "bb",         "cc",       "dd",     9.6,       11,         
  "a",         "bbb",        "cc",       "dd",     3.3,       5,        
  "a",         "bbbb",       "cc",       "dd",     7.9,       2,        
  "aa",        "b",          "cc",       "dd",     9.9,       1,         
  "aa",        "bb",         "cc",       "dd",     21.3,      3,          
  "aa",        "bbb",        "cc",       "dd",     14.2,      8,         
  "aa",        "bbbb",       "cc",       "dd",     9.5,       110,
  "a",         "b",          "ccc",      "dd",     5.5,       9,          
  "a",         "bb",         "ccc",      "dd",     1.5,       3,         
  "a",         "bbb",        "ccc",      "dd",     3.3,       2,        
  "a",         "bbbb",       "ccc",      "dd",     7.9,       9,        
  "aa",        "b",          "ccc",      "dd",     1.2,       5,         
  "aa",        "bb",         "ccc",      "dd",     11.3,      9,          
  "aa",        "bbb",        "ccc",      "dd",     22.2,      18,         
  "aa",        "bbbb",       "ccc",      "dd",     1.5,       95,    
)






# All automatic tests will not save an excel_file

test_that("simple 1 level suppression, and making sure default sample_size_col is working", {
  expect_named(fix_suppression(df = test_df,groups = list("group_a"), save_excel_file = FALSE ),
               c("group_a", "group_b", "group_c", "value", "sample_size",  "nobs"
               ))
  expect_equal(pull(fix_suppression(df = test_df,groups = list("group_a"), save_excel_file = FALSE ),
                    "sample_size"), c(2, 3, 10, 12, 1, 1, 1, 152))
  expect_equal(pull(fix_suppression(df = test_df,groups = list("group_b"), save_excel_file = FALSE ),
                    "sample_size"), c(2, 3, 10, 12, 1, 3, 8, 152)) # This one should not change anything
  })

test_that("1 level suppression and there is one orphan level (e.g. only one row for that level)", {
  expect_named(fix_suppression(df = test_df,groups = list("group_c"), save_excel_file = FALSE ),
               c("group_a", "group_b", "group_c", "value", "sample_size",  "nobs"
               ))
  expect_equal(pull(fix_suppression(df = test_df,groups = list("group_c"), save_excel_file = FALSE ),
                    "sample_size"), c(2, 1, 1, 12, 1, 1, 1, 152))
})

test_that("simple 2 level suppression", {
  expect_named(fix_suppression(df = test_df,groups = list("group_a","group_b"), save_excel_file = FALSE ),
               c("group_a", "group_b", "group_c", "value", "sample_size",  "nobs"
               ))
  expect_equal(pull(fix_suppression(df = test_df,groups = list("group_a","group_b"), save_excel_file = FALSE ),
                    "sample_size"), c(2, 3, 1, 12, 1, 1, 1, 152))
  })

test_that("simple 2 level suppression, and while using a custom sample_size column name", {
  expect_named(fix_suppression(df = test_df,groups = list("group_a","group_b"),sample_size_col = "nobs", save_excel_file = FALSE ),
               c("group_a", "group_b", "group_c", "value", "sample_size",  "nobs"
               ))
  expect_equal(pull(fix_suppression(df = test_df,groups = list("group_a","group_b"), sample_size_col = "nobs", save_excel_file = FALSE ),
                    "nobs"), c(1, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(pull(fix_suppression(df = test_df %>% dplyr::rename(test_nobs = sample_size),
                                             groups = list("group_a","group_b"), 
                                             sample_size_col = "test_nobs", save_excel_file = FALSE),
                    "test_nobs"), c(2, 3, 1, 12, 1, 1, 1, 152))
  })


test_that("2 level suppression, with each group being made of multiple columns", {
  expect_named(fix_suppression(df = test_df_complex,groups = list(c("group_d","group_c","group_b"),c("group_d","group_c","group_a")), save_excel_file = FALSE ),
               c("group_a", "group_b", "group_c", "group_d","value", "sample_size"
                 ))
  expect_equal(pull(fix_suppression(df = test_df_complex,groups = list(c("group_d","group_c","group_b"),c("group_d","group_c","group_a")), save_excel_file = FALSE ),
                    "sample_size"), c(2, 3, 1, 12, 1, 1, 1, 152, 1, 1, 5, 1, 1, 1, 8, 1, 9, 1, 1, 9, 5, 1, 1, 95))
  })


# Manual test, will be commented out but this will test that exporting the excel file works

# fix_suppression(df = test_df_complex,
#                          groups = list(c("group_d","group_c","group_b"),
#                                        c("group_d","group_c","group_a")),
#                          export_path = "C:/Users/fc000033/OneDrive - Defra/Desktop",
#                          file_name = "testtttttt",
#                          save_excel_file = TRUE )








