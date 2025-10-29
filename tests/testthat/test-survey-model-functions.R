set.seed(1)

test_df <- dplyr::tibble(
  id = 1:500,
  group = c(rep("a", 100), rep("b", 100), rep("c", 100), rep("d", 100), rep("e", 100)),
  n_pop = c(rep(500, 100), rep(600, 100), rep(200, 100), rep(400, 100), rep(200, 100)),
  weight = runif(500, 1, 10),
  question = runif(500, 2, 200),
  binary_question = dplyr::if_else(question < 100, 0, 1),
  random_predictor = runif(500, 10, 150),
  linear_predictor = dplyr::case_match(group, "a" ~ question * 1.02,
                                       "b" ~ question * 1.5,
                                       "c" ~ question / 0.95,
                                       "d" ~ question * 1.1,
                                       "e" ~ question * 2)
)

# Summarise variables ####
test_that("summarise_variables returns expected columns", {
  expect_named(summarise_variables(test_df, "question"), c("check", "result"))
  expect_named(summarise_variables(test_df, "binary_question"), c("check", "result"))
})

test_that("summarise_variables returns different results for linear and binary variables", {
  expect_false(isTRUE(all.equal(summarise_variables(test_df, "question")$check,
                                summarise_variables(test_df, "binary_question")$check)))
})

# Normalise values ####
test_that("normalise_values returns a vector", {
  expect_type(normalise_values(test_df, "question"), "double")
})

test_that("normalise_values returns an warning with a binary variable", {
  expect_warning(normalise_values(test_df, "binary_question"),
                 "binary_question is binary, no need to transform")
})

test_that("normalise_values creates file when specified", {
  plot_dir <- tempdir()
  plot_file <- paste0(plot_dir, "\\question_transformation.png")
  on.exit(unlink(plot_file))
  expect_false(file.exists(plot_file))
  normalise_values(test_df, "question", save_path = plot_dir)
  expect_true(file.exists(plot_file))
})

# Wald test ####
test_design <- srvyr::as_survey_design(test_df, id = id, strata = group,
                                       fpc = n_pop, weight = weight, nest = TRUE)

test_that("run_wald_test returns a tibble", {
  expect_s3_class(run_wald_test(survey::svyglm(design = test_design,
                                               formula = question ~ random_predictor)),
                  "tbl_df")
})

test_that("run_wald_test returns expected results", {
  expect_gt(run_wald_test(survey::svyglm(design = test_design,
                                           formula = question ~ random_predictor)) %>%
              pull(p) %>% as.numeric(), 0.5)
  expect_lt(run_wald_test(survey::svyglm(design = test_design,
                                         formula = question ~ linear_predictor)) %>%
              pull(p) %>% as.numeric(), 0.05)
  expect_gt(run_wald_test(survey::svyglm(design = test_design,
                                         formula = binary_question ~ random_predictor,
                                         family = "quasibinomial")) %>%
              pull(p) %>% as.numeric(), 0.5)
  expect_lt(run_wald_test(survey::svyglm(design = test_design,
                                         formula = binary_question ~ linear_predictor,
                                         family = "quasibinomial")) %>%
              pull(p) %>% as.numeric(), 0.05)
})

# Model performance ####
test_that("test_model_performance calculates correct statistic", {
  expect_named(test_model_performance(survey::svyglm(design = test_design,
                                                     formula = question ~ random_predictor))[[2]],
               "pearson.coef")
  expect_named(test_model_performance(survey::svyglm(design = test_design,
                                                     formula = binary_question ~ random_predictor,
                                                     family = "quasibinomial"))[[2]],
               c("calc", "value"))
})

test_that("test_model_performance returns expected Pearson results", {
  expect_true(dplyr::between(
    test_model_performance(survey::svyglm(design = test_design,
                                          formula = question ~ random_predictor))[[2]],
    -.1, .1))
  expect_gt(test_model_performance(survey::svyglm(design = test_design,
                                                     formula = question ~ linear_predictor))[[2]],
            0.8)
  expect_gt(test_model_performance(survey::svyglm(design = test_design,
                                                  formula = question ~ linear_predictor + group))[[2]],
            0.9)
})

test_that("test_model_performance returns expected TSS results", {
  expect_true(dplyr::between(
    test_model_performance(survey::svyglm(design = test_design,
                                          formula = binary_question ~ random_predictor,
                                          family = "quasibinomial"))[[2]] %>%
      dplyr::pull(value) %>% dplyr::last(),
    -.1, .1))
  expect_gt(test_model_performance(survey::svyglm(design = test_design,
                                                  formula = binary_question ~ linear_predictor,
                                                  family = "quasibinomial"))[[2]] %>%
              dplyr::pull(value) %>% dplyr::last(),
            0.6)
})

test_that("test_model_performance creates file when specified", {
  plot_dir <- tempdir()
  plot_file <- paste0(plot_dir, "\\question_by_linear_predictor_and_group.png")
  on.exit(unlink(plot_file))
  expect_false(file.exists(plot_file))
  test_model_performance(survey::svyglm(design = test_design, formula = question ~ linear_predictor + group),
                         save_path = plot_dir)
  expect_true(file.exists(plot_file))
})
