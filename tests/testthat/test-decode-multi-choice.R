test_df <- tibble(
  id = 1:5,
  question_one = c(17, 9, 5, 3, 42),
  question_two = c(24, 29, 1, 33, 12)
)

test_that("decomposing value works", {
  expect_equal(decompose_multi_choice_value(6), "2; 4")
  expect_equal(decompose_multi_choice_value(17), "1; 16")
  expect_equal(decompose_multi_choice_value(2), "2")
  expect_equal(decompose_multi_choice_value(42), "2; 8; 32")
})

test_that("decomposing vector works", {
  expect_equal(decompose_multi_choice_column(test_df$question_one),
               list(c("1; 16"), c("1; 8"), c("1; 4"), c("1; 2"), c("2; 8; 32")))
})

test_that("decomposing one column works", {
  expect_named(decode_multi_choice_column(test_df, "question_one", "id"),
               c("id", "question_two", "question_one_1",
                 "question_one_2", "question_one_4", "question_one_8",
                 "question_one_16", "question_one_32"))
  expect_equal(pull(decode_multi_choice_column(test_df, "question_one", "id"),
                    "question_one_1"), c(1, 1, 1, 1, NA))
})

test_that("decomposing two column returns an error", {
  expect_error(decode_multi_choice_column(test_df, c("question_one", "question_two"), "id"),
               "Only decode one column at a time")
})
