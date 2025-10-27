test_df <- tibble(
  id = 1:10,
  id_1_part = rep(1:5,2),
  id_2_part = c(rep("a",5),rep("b",5)),
  question_one = c(17, 9, 5, 3, 42, 2, 1, 10, 128, 200),
  question_two = c(24, 29, 1, 33, 12, 1200, 2548, 1024, 2, 1),
  question_three = c(1024, 512, 248, 612, 18, 16, 24, 33, 1024, 512 )
)

test_list <- list(
  question_one_codes = tidyr::tribble(
    ~ "code", ~ "description",
    0, "answer_q1_0",
    1, "answer_q1_1",
    2, "answer_q1_2",
    4, "answer_q1_3",
    8, "answer_q1_4",
    16, "answer_q1_5",
    32, "answer_q1_6",
    64, "answer_q1_7",
    128, "answer_q1_8"
    ),
  question_two_codes = tidyr::tribble(
    ~ "code", ~ "description",
    1, "answer_q2_1",
    2, "answer_q2_2",
    4, "answer_q2_3",
    8, "answer_q2_4",
    16, "answer_q2_5",
    32, "answer_q2_6",
    64, "answer_q2_7",
    128, "answer_q2_8",
    256, "answer_q2_9",
    512, "answer_q2_10",
    1024, "answer_q2_11",
    2048, "answer_q2_12"
    ),
  question_three_codes = tidyr::tribble(
    ~ "code", ~ "description",
    1, "answer_q3_1",
    2, "answer_q3_2",
    4, "answer_q3_3",
    8, "answer_q3_4",
    16, "answer_q3_5",
    32, "answer_q3_6",
    64, "answer_q3_7",
    128, "answer_q3_8",
    256, "answer_q3_9",
    512, "answer_q3_10",
    1024, "answer_q3_11",
    2048, "answer_q3_12"
  )
)



test_that("decomposing value works", {
  expect_equal(decompose_multi_choice_value(6), "2; 4")
  expect_equal(decompose_multi_choice_value(17), "1; 16")
  expect_equal(decompose_multi_choice_value(2), "2")
  expect_equal(decompose_multi_choice_value(42), "2; 8; 32")
})

test_that("decomposing vector works", {
  expect_equal(decompose_multi_choice_column(test_df$question_one),
               list(c("1; 16"), c("1; 8"), c("1; 4"), c("1; 2"), c("2; 8; 32"),
                    c("2"), c("1"), c("2; 8"), c("128"), c("8; 64; 128")
                    ))
  })

test_that("codes not the same length of variables", {
  expect_error(decode_multi_choice_column(test_df,
                                          var = c("question_one"),
                                          id_cols = "id", 
                                          codes = c((test_list$question_one_codes),
                                                    (test_list$question_two_codes))
                                          )
               )
  })

test_that("codes must have the correct colnames", {
  expect_error(decode_multi_choice_column(test_df,
                                          var = c("question_one"),
                                          id_cols = "id", 
                                          codes = test_list$question_one_codes %>%
                                                       dplyr::rename(despicable_me = description)
                                          )
               )
  expect_error(decode_multi_choice_column(test_df,
                                          var = c("question_one"),
                                          id_cols = "id", 
                                          codes = test_list$question_one_codes %>%
                                            dplyr::rename(despicable_me_2 = code)
                                          )
               )
  })


test_that("codes not the same length of variables", {
  expect_error(decode_multi_choice_column(test_df,
                                          var = c("question_one"),
                                          id_cols = "id", 
                                          codes = c((test_list$question_one_codes),
                                                    (test_list$question_two_codes))
                                          )
               )
})

test_that("without codes code_descriptions option will not work in colnames_option", {
  expect_error(decode_multi_choice_column(test_df,
                                          var = c("question_one"),
                                          id_cols = "id", 
                                          colnames_option = "code_descriptions"
                                          )
               )
  })

test_that("without codes code_descriptions option will not work in data_option", {
  expect_error(decode_multi_choice_column(test_df,
                                          var = c("question_one"),
                                          id_cols = "id",
                                          data_option = "code_descriptions"
                                          )
               )
  })



test_that("decomposing one column works", {
  expect_named(decode_multi_choice_column( test_df, var = "question_one",id_cols = "id"),
               c("id", "id_1_part", "id_2_part", "question_one", "question_two", "question_three", "question_one__1",
                 "question_one__2", "question_one__4", "question_one__8",
                 "question_one__16", "question_one__32", "question_one__64", "question_one__128"
                 ))
  expect_equal(pull(decode_multi_choice_column(test_df, "question_one", "id"),
                    "question_one__1"), c(1, 1, 1, 1, NA, NA, 1, NA, NA, NA))
  })


test_that("decomposing two column works", {
  expect_named(decode_multi_choice_column( test_df, var = c("question_one","question_two"),id_cols = "id"),
               c("id", "id_1_part", "id_2_part", "question_one", "question_two", "question_three", 
                 "question_one__1", "question_one__2", "question_one__4", "question_one__8",
                 "question_one__16", "question_one__32", "question_one__64", "question_one__128",
                 "question_two__1", "question_two__2", "question_two__4", "question_two__8",
                 "question_two__16", "question_two__32", "question_two__64", "question_two__128",
                 "question_two__256", "question_two__1024", "question_two__2048"
               ))
  expect_equal(pull(decode_multi_choice_column(test_df, c("question_one","question_two"), "id"),
                    "question_one__1"), c(1, 1, 1, 1, NA, NA, 1, NA, NA, NA))
  expect_equal(pull(decode_multi_choice_column(test_df, c("question_one","question_two"), "id"),
                    "question_two__1"), c(NA, 1, 1, 1, NA, NA, NA, NA, NA, 1))
  })


test_that("decomposing three column works, with colnames_option = `code_descriptions`", {
  expect_named(decode_multi_choice_column( test_df,
                                           var = c("question_one","question_two","question_three"),
                                           id_cols = "id", 
                                           codes = test_list,
                                           colnames_option = "code_descriptions"
                                           ),
               c("id", "id_1_part", "id_2_part", "question_one", "question_two", 
                 "question_three", "question_one__answer_q1_0", "question_one__answer_q1_1", 
                 "question_one__answer_q1_2", "question_one__answer_q1_3", "question_one__answer_q1_4", 
                 "question_one__answer_q1_5", "question_one__answer_q1_6", "question_one__answer_q1_7", 
                 "question_one__answer_q1_8", "question_two__No answer selected", "question_two__answer_q2_1", 
                 "question_two__answer_q2_2", "question_two__answer_q2_3", "question_two__answer_q2_4", 
                 "question_two__answer_q2_5", "question_two__answer_q2_6", "question_two__answer_q2_7", 
                 "question_two__answer_q2_8", "question_two__answer_q2_9", "question_two__answer_q2_10", 
                 "question_two__answer_q2_11", "question_two__answer_q2_12", "question_three__No answer selected",
                 "question_three__answer_q3_1", "question_three__answer_q3_2", "question_three__answer_q3_3",
                 "question_three__answer_q3_4", "question_three__answer_q3_5", "question_three__answer_q3_6",
                 "question_three__answer_q3_7", "question_three__answer_q3_8", "question_three__answer_q3_9",
                 "question_three__answer_q3_10", "question_three__answer_q3_11", "question_three__answer_q3_12"  
               ))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = "id", 
                                               codes = test_list,
                                               colnames_option = "code_descriptions"
                                               ),
                    "question_one__answer_q1_1"), 
               c(1, 1, 1, 1, NA, NA, 1, NA, NA, NA))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = "id", 
                                               codes = test_list,
                                               colnames_option = "code_descriptions"
                                               ),
                    "question_two__No answer selected"),
               as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)))
})




test_that("decomposing three column works, with data_option = `code_descriptions`", {
  expect_named(decode_multi_choice_column( test_df,
                                           var = c("question_one","question_two","question_three"),
                                           id_cols = "id", 
                                           codes = test_list,
                                           data_option = "code_descriptions"
                                           ),
               c("id", "id_1_part", "id_2_part", "question_one", "question_two", "question_three", "question_one__0",
                 "question_one__1", "question_one__2", "question_one__4", "question_one__8", "question_one__16", 
                 "question_one__32", "question_one__64", "question_one__128", "question_two__1", "question_two__2",
                 "question_two__4", "question_two__8", "question_two__16", "question_two__32", "question_two__64",
                 "question_two__128", "question_two__256", "question_two__512", "question_two__1024", "question_two__2048",
                 "question_three__1", "question_three__2", "question_three__4", "question_three__8", "question_three__16",
                 "question_three__32", "question_three__64", "question_three__128", "question_three__256", "question_three__512", 
                 "question_three__1024", "question_three__2048"
                 ))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = "id", 
                                               codes = test_list,
                                               data_option = "code_descriptions"
                                               ),
                    "question_one__1"), 
               c("answer_q1_1", "answer_q1_1", "answer_q1_1", "answer_q1_1", "not selected", 
                 "not selected", "answer_q1_1", "not selected", "not selected", "not selected"))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = "id", 
                                               codes = test_list,
                                               data_option = "code_descriptions"
                                               ),
                    "question_two__16"),
               c("answer_q2_4", "answer_q2_4", "not selected", "not selected", "not selected", "answer_q2_4",
                 "answer_q2_4", "not selected", "not selected", "not selected"))
  })




test_that("decomposing three column works, with data_option = `binary`", {
  expect_named(decode_multi_choice_column( test_df,
                                           var = c("question_one","question_two","question_three"),
                                           id_cols = "id", 
                                           codes = test_list,
                                           data_option = "binary"
                                           ),
               c("id", "id_1_part", "id_2_part", "question_one", "question_two", "question_three", "question_one__0",
                 "question_one__1", "question_one__2", "question_one__4", "question_one__8", "question_one__16", 
                 "question_one__32", "question_one__64", "question_one__128", "question_two__1", "question_two__2",
                 "question_two__4", "question_two__8", "question_two__16", "question_two__32", "question_two__64",
                 "question_two__128", "question_two__256", "question_two__512", "question_two__1024", "question_two__2048",
                 "question_three__1", "question_three__2", "question_three__4", "question_three__8", "question_three__16",
                 "question_three__32", "question_three__64", "question_three__128", "question_three__256", "question_three__512", 
                 "question_three__1024", "question_three__2048"
                 ))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = "id", 
                                               codes = test_list,
                                               data_option = "binary"
                                               ),
                    "question_one__1"), 
               c(1, 1, 1, 1, 0, 0, 1, 0, 0, 0))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = "id", 
                                               codes = test_list,
                                               data_option = "binary"
                                               ),
                    "question_two__16"),
               c(1, 1, 0, 0, 0, 1, 1, 0, 0, 0))
})



test_that("decomposing three column works, test with 2 id columns", {
  expect_named(decode_multi_choice_column(test_df,
                                          var = c("question_one","question_two","question_three"),
                                          id_cols = c("id_1_part","id_2_part"),
                                          codes = test_list,
                                          data_option = "binary",
                                          colnames_option = "code_descriptions"),
               c("id", "id_1_part", "id_2_part", "question_one", "question_two", 
                 "question_three", "question_one__answer_q1_0", "question_one__answer_q1_1", 
                 "question_one__answer_q1_2", "question_one__answer_q1_3", "question_one__answer_q1_4", 
                 "question_one__answer_q1_5", "question_one__answer_q1_6", "question_one__answer_q1_7", 
                 "question_one__answer_q1_8", "question_two__No answer selected", "question_two__answer_q2_1", 
                 "question_two__answer_q2_2", "question_two__answer_q2_3", "question_two__answer_q2_4", 
                 "question_two__answer_q2_5", "question_two__answer_q2_6", "question_two__answer_q2_7", 
                 "question_two__answer_q2_8", "question_two__answer_q2_9", "question_two__answer_q2_10", 
                 "question_two__answer_q2_11", "question_two__answer_q2_12", "question_three__No answer selected",
                 "question_three__answer_q3_1", "question_three__answer_q3_2", "question_three__answer_q3_3",
                 "question_three__answer_q3_4", "question_three__answer_q3_5", "question_three__answer_q3_6",
                 "question_three__answer_q3_7", "question_three__answer_q3_8", "question_three__answer_q3_9",
                 "question_three__answer_q3_10", "question_three__answer_q3_11", "question_three__answer_q3_12" 
                 
               ))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = c("id_1_part","id_2_part"),
                                               codes = test_list,
                                               data_option = "binary",
                                               colnames_option = "code_descriptions"),
                    "question_one__answer_q1_1"), 
               c(1, 1, 1, 1, 0, 0, 1, 0, 0, 0))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = c("id_1_part","id_2_part"),
                                               codes = test_list,
                                               data_option = "binary",
                                               colnames_option = "code_descriptions"),
                    "question_two__answer_q2_5"),
               c(1, 1, 0, 0, 0, 1, 1, 0, 0, 0))
})

test_that(paste0("When using colnames_option = `code_descriptions` and/or data_option = `code_descriptions`, ",
                 "correctly keep the 0 code name when that is in the code list, and create a 0 code description, 
                 when is isn't in the list of codes"), {
  expect_named(decode_multi_choice_column(test_df,
                                          var = c("question_one","question_two","question_three"),
                                          id_cols = c("id_1_part","id_2_part"),
                                          codes = test_list,
                                          data_option = "binary",
                                          colnames_option = "code_descriptions"),
               c("id", "id_1_part", "id_2_part", "question_one", "question_two", 
                 "question_three", "question_one__answer_q1_0", "question_one__answer_q1_1", 
                 "question_one__answer_q1_2", "question_one__answer_q1_3", "question_one__answer_q1_4", 
                 "question_one__answer_q1_5", "question_one__answer_q1_6", "question_one__answer_q1_7", 
                 "question_one__answer_q1_8", "question_two__No answer selected", "question_two__answer_q2_1", 
                 "question_two__answer_q2_2", "question_two__answer_q2_3", "question_two__answer_q2_4", 
                 "question_two__answer_q2_5", "question_two__answer_q2_6", "question_two__answer_q2_7", 
                 "question_two__answer_q2_8", "question_two__answer_q2_9", "question_two__answer_q2_10", 
                 "question_two__answer_q2_11", "question_two__answer_q2_12", "question_three__No answer selected",
                 "question_three__answer_q3_1", "question_three__answer_q3_2", "question_three__answer_q3_3",
                 "question_three__answer_q3_4", "question_three__answer_q3_5", "question_three__answer_q3_6",
                 "question_three__answer_q3_7", "question_three__answer_q3_8", "question_three__answer_q3_9",
                 "question_three__answer_q3_10", "question_three__answer_q3_11", "question_three__answer_q3_12" 
                 
               ))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = c("id_1_part","id_2_part"),
                                               codes = test_list,
                                               data_option = "binary",
                                               colnames_option = "code_descriptions"),
                    "question_one__answer_q1_1"), 
               c(1, 1, 1, 1, 0, 0, 1, 0, 0, 0))
  expect_equal(pull(decode_multi_choice_column(test_df,
                                               var = c("question_one","question_two","question_three"),
                                               id_cols = c("id_1_part","id_2_part"),
                                               codes = test_list,
                                               data_option = "binary",
                                               colnames_option = "code_descriptions"),
                    "question_two__answer_q2_5"),
               c(1, 1, 0, 0, 0, 1, 1, 0, 0, 0))
})














