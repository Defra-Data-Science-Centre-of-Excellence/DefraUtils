# Functions for modelling relationships between survey variables

#' Summarise variable to be modelled
#'
#' @importFrom dplyr tribble
#' @importFrom moments skewness
#'
#' @param df A data set
#' @param column String; column to be summarised
#'
#' @family survey modelling functions
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export
summarise_variables <- function(df, column) {

  values <- pull(df, !!sym(column))[!is.na(pull(df, !!sym(column)))]

  if (length(unique(values)) == 2) {

    binary <- sort(unique(values))

    tribble(
      ~check,                                      ~result,
      paste("Rows where", column, "=", binary[1]), sum(values == binary[1], na.rm = T),
      paste("Rows where", column, "=", binary[2]), sum(values == binary[2], na.rm = T),
      paste("Skewness of", column, "values"),      skewness(values, na.rm = T))

  } else {

    tribble(
      ~check,                                           ~result,
      paste("Rows where", column, "< 0"),               sum(values < 0, na.rm = T),
      paste("Rows where", column, "= 0"),               sum(values == 0, na.rm = T),
      paste("Lowest", column, "value"),                 min(values, na.rm = T),
      paste("25%", column, "quantile"),                 quantile(values, .25, na.rm = T),
      paste("50%", column, "quantile"),                 quantile(values, .5, na.rm = T),
      paste("75%", column, "quantile"),                 quantile(values, .75, na.rm = T),
      paste("Highest", column, "value"),                max(values, na.rm = T),
      paste("Skewness of", column, "values"),           skewness(values, na.rm = T))

  }

}

#' Normalise values and check result
#'
#' Uses [bestNormalize::bestNormalize()] to select the best normalising
#' transformation for a vector of numeric values, then performs this
#' transformation. Will also creates a plot with two charts showing the
#' distributions of the untransformed and transformed variables, including the
#' name of the transformation used and the skewness value of the result. If
#' `save_path` is set, the plot will be saved as a PNG to the specified directory.
#'
#' @importFrom bestNormalize bestNormalize
#' @importFrom moments skewness
#'
#' @param df A dataframe with a column you are running a model on
#' @param model_col String; the name of the column you are running a model on
#' @param save_path The directory where the plot should be saved to
#' @param leave_one_out Passed to the `loo` argument of [bestNormalize::bestNormalize()]
#' @param ... Other arguments passed to [bestNormalize::bestNormalize()]
#'
#' @family survey modelling functions
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export
normalise_values <- function(df, model_col, save_path = NULL,
                             leave_one_out = TRUE, ...) {

  if (length(unique(df[[model_col]])) == 2) {
    warning(paste(model_col, "is binary, no need to transform"))
  }

  bestNormalize_output <- bestNormalize(df[[model_col]], loo = leave_one_out, ...)
  untransformed_col <- df[[model_col]]
  transformed_col <- predict(bestNormalize_output)

  if (!is.null(save_path)) {
    png(file = paste0(save_path, "\\", model_col, "_transformation.png"),
        width = 600, height = 600)
  }

  par(mfrow = c(2,1))
  plot(density(untransformed_col, na.rm = T),
       main = paste("untransformed", model_col),
       sub = paste("Skewness =", skewness(untransformed_col, na.rm = T)))
  plot(density(transformed_col, na.rm = T),
       main = paste(names(sort(bestNormalize_output$norm_stats)[1]), "transformation"),
       sub = paste("Skewness =", skewness(transformed_col, na.rm = T)))

  if (!is.null(save_path)) {
    dev.off()
  }

  return(transformed_col)

}

#' Wald test
#'
#' Perform the Wald test on a survey model with [survey::regTermTest()].
#'
#' @param survey_model A svyglm object
#'
#' @importFrom survey regTermTest
#' @importFrom dplyr as_tibble arrange desc across
#' @importFrom plyr round_any
#'
#' @family survey modelling functions
#'
#' @seealso Tested on models created with [survey::svyglm()]
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export
run_wald_test <- function(survey_model) {

  responses <- names(survey_model$model[-1])[1:(length(names(survey_model$model[-1]))-1)] %>%
    lapply(\(x) unlist(regTermTest(survey_model, x, method = "Wald")[3:7])) %>%
    bind_rows() %>%
    mutate(across(c(Ftest, df, ddf, p), ~format(round_any(as.numeric(.x), 1e-4),
                                                scientific = FALSE, trim = T))) %>%
    arrange(desc(p))

  return(responses)

}

#' Check the sensitivity and specificity of a svyglm model
#'
#' Currently only takes models with a continuous or binary response variable;
#' could be adapted for models with categorical response variables. If
#' `save_path` is set, the plot will be saved as a PNG to the specified directory.
#'
#' @importFrom dplyr filter as_tibble slice_sample setdiff mutate select
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 ggplot aes geom_point geom_boxplot labs ggsave
#' @importFrom survey svyglm
#' @importFrom modelr add_predictions
#'
#' @param survey_model A svyglm object
#' @param save_path The directory where the plot should be saved to
#'
#' @family survey modelling functions
#'
#' @seealso Tested on models created with [survey::svyglm()]
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export
test_model_performance <- function(survey_model, save_path = NULL) {

  set.seed(1)

  model_var <- as.character(formula(survey_model))[[2]]
  predictor_var <- as.character(formula(survey_model))[[3]]

  file_name <- paste0(model_var, "_by_",
                      str_replace_all(predictor_var, " [\\+|\\*] ", "_and_"),
                      ".png")

  # Only use data where the response variable isn't NA
  temp_data <- if ("data" %in% names(survey_model)) {
    as_tibble(survey_model$data) %>%
      filter(!is.na(!! sym(model_var)))
  } else if ("design" %in% names(survey_model)) {
    as_tibble(survey_model$design) %>%
      filter(!is.na(!! sym(model_var)))
  } else {
    stop("Unable to extract survey data")
  }

  # Set test and train data
  training_data <- slice_sample(temp_data, prop = 0.8)
  testing_data <- setdiff(temp_data, training_data)

  # Fit survey_model to training data
  new_model <- tryCatch(
    expr = {
      update(survey_model, data = training_data)
    },
    error = function(e){
      stop("Unable to update survey model")
    }
  )

  # Get predictions
  responses <- testing_data %>%
    add_predictions(new_model, var = "y", type = "response") %>%
    mutate(y = as.numeric(y)) %>%
    select(x = !! sym(model_var), y)

  # Get performance
  if (is.numeric(temp_data[[model_var]]) & length(unique(temp_data[[model_var]])) > 2) {

    # if response is continuous, return a scatter plot and calculate Pearson's coefficient
    p <- ggplot(responses, aes(x = x, y = y)) +
      geom_point() +
      labs(x = paste("actual", model_var, "response"),
           y = paste0("predicted response (based on ", predictor_var, ")"))

    if (!is.null(save_path)) {
      ggsave(file.path(save_path, file_name), p, width = 6, height = 6,
             device = "png", units = "in")
    }

    correlation <- c(pearson.coef = cor(responses$x, responses$y, method = c("pearson")))

    return(list(p, correlation))

  } else if (length(unique(temp_data[[model_var]])) == 2) {

    # if response is binary, return a boxplot and calculate True Skill Statistic
    p <- ggplot(responses, aes(x = as.character(x), y = y)) +
      geom_boxplot() +
      labs(x = paste("actual", model_var, "response"),
           y = paste0("predicted response (based on ", predictor_var, ")"))

    if (!is.null(save_path)) {
      ggsave(file.path(save_path, file_name), p, width = 6, height = 6,
             device = "png", units = "in")
    }

    # True positives
    true_positives <- length(responses$y[responses$x == TRUE & responses$y > 0.5])
    true_positives_rate <- true_positives/length(responses$x[responses$x == TRUE])
    # False positives
    false_positives <- length(responses$y[responses$x == FALSE & responses$y > 0.5])
    false_positives_rate <- false_positives/length(responses$x[responses$x == FALSE])
    # True negatives
    true_negatives <- length(responses$y[responses$x == FALSE & responses$y < 0.5])
    true_negatives_rate <- true_negatives/length(responses$x[responses$x == FALSE])
    # False negatives
    false_negatives <- length(responses$y[responses$x == TRUE & responses$y < 0.5])
    false_negatives_rate <- false_negatives/length(responses$x[responses$x == TRUE])
    # true skill statistic (TSS)
    true_skill_statistic <- (true_positives / (true_positives + false_negatives)) +
      (true_negatives / (false_positives + true_negatives)) - 1

    tss_calc <- tribble(
      ~calc, ~value,
      "True positives",       length(responses$y[responses$x == TRUE & responses$y > 0.5]),
      "True positives rate",  true_positives/length(responses$x[responses$x == TRUE]),
      "False positives",      length(responses$y[responses$x == FALSE & responses$y > 0.5]),
      "False positives rate", false_positives/length(responses$x[responses$x == FALSE]),
      "True negatives",       length(responses$y[responses$x == FALSE & responses$y < 0.5]),
      "True negatives rate",  true_negatives/length(responses$x[responses$x == FALSE]),
      "False negatives",      length(responses$y[responses$x == TRUE & responses$y < 0.5]),
      "False negatives rate", false_negatives/length(responses$x[responses$x == TRUE]),
      "True Skill Statistic", (true_positives / (true_positives + false_negatives)) +
        (true_negatives / (false_positives + true_negatives)) - 1
    )

    return(list(p, tss_calc))

  } else {

    # if response is not binary or continuous, return an error
    print("can't handle this type of model")

  }
}
