# Functions to deal with multiple choice questions

#' Decompose multiple choice value
#'
#' When a survey, such as the Farm Business Survey, requires all answers to be
#' input as numbers, a geometric sequence (1, 2, 4, 8, 16, etc.) is used to code
#' multiple choice answers.
#' For example, the respondent choosing the first (code 1) and third (code 4)
#' options would be input as 5.
#' This method means that any combination of choices can be decoded into its
#' constituent parts.
#' This function decodes these answers by taking an input value and
#' returning separate values for each answer.
#'
#' @param n Value to decompose
#'
#' @export
#'
#' @family functions for decoding multiple choice answers
decompose_multi_choice_value <- function(n) {

  powers <- integer(0)

  while (n > 0) {
    power <- floor(log2(n))
    powers <- c(powers, 2^power)
    n <- n - 2^power
  }

  powers <- sort(powers)
  result <- paste(powers, collapse = "; ")

  return(result)

}

#' Apply decompose_multi_choice_value to a column
#'
#' When a survey, such as the Farm Business Survey, requires all answers to be
#' input as numbers, a geometric sequence (1, 2, 4, 8, 16, etc.) is used to code
#' multiple choice answers.
#' For example, the respondent choosing the first (code 1) and third (code 4)
#' options would be input as 5.
#' This method means that any combination of choices can be decoded into its
#' constituent parts.
#' This function decodes these answers by taking an input column of values and
#' returning separate values for each answer.
#'
#' @param column Column to decompose
#'
#' @export
#'
#' @family functions for decoding multiple choice answers
decompose_multi_choice_column <- function(column) {
  decoded <- lapply(column, decompose_multi_choice_value)
  return(decoded)
}

#' Decode a multiple choice column
#'
#' When a survey, such as the Farm Business Survey, requires all answers to be
#' input as numbers, a geometric sequence (1, 2, 4, 8, 16, etc.) is used to code
#' multiple choice answers.
#' For example, the respondent choosing the first (code 1) and third (code 4)
#' options would be input as 5.
#' This method means that any combination of choices can be decoded into its
#' constituent parts.
#' This function decodes these answers by taking an input dataset with a column
#' of values and returning separate columns for each answer.
#'
#' @importFrom dplyr %>% select mutate na_if filter arrange left_join
#' @importFrom tidyr pivot_longer
#' @importFrom rlang set_names
#'
#' @param input_data Dataset
#' @param var String; Column to decode
#' @param id_cols String; All columns used to uniquely identify the row
#'
#' @export
#'
#' @family functions for decoding multiple choice answers
decode_multi_choice_column <- function(input_data, var, id_cols) {

  # TO DO: allow decoding of multiple columns
  if (length(var) > 1) {
    stop("length(var) > 1: Only decode one column at a time")
  }

  decoded_data <- input_data %>%
    select(all_of(c(id_cols, var))) %>%
    mutate(chosen_codes = decompose_multi_choice_column(.[[var]])) %>%
    mutate(`0` = ifelse(.[[var]] == 0, 1, 0),
           `1` = ifelse(grepl("\\b1\\b", chosen_codes), 1, 0),
           `2` = ifelse(grepl("\\b2\\b", chosen_codes), 1, 0),
           `4` = ifelse(grepl("\\b4\\b", chosen_codes), 1, 0),
           `8` = ifelse(grepl("\\b8\\b", chosen_codes), 1, 0),
           `16` = ifelse(grepl("\\b16\\b", chosen_codes), 1, 0),
           `32` = ifelse(grepl("\\b32\\b", chosen_codes), 1, 0),
           `64` = ifelse(grepl("\\b64\\b", chosen_codes), 1, 0),
           `128` = ifelse(grepl("\\b128\\b", chosen_codes), 1, 0),
           `256` = ifelse(grepl("\\b256\\b", chosen_codes), 1, 0),
           `512` = ifelse(grepl("\\b512\\b", chosen_codes), 1, 0),
           `1024` = ifelse(grepl("\\b1024\\b", chosen_codes), 1, 0),
           `2048` = ifelse(grepl("\\b2048\\b", chosen_codes), 1, 0),
           across(c("0", "1", "2", "4", "8", "16", "32", "64",
                    "128", "256", "512", "1024", "2048"),
                  ~as.numeric(na_if(.x, 0)))) %>%
    select(-all_of(c(var, "chosen_codes"))) %>%
    pivot_longer(c("0", "1", "2", "4", "8", "16", "32", "64",
                   "128", "256", "512", "1024", "2048"),
                 names_to = "code") %>%
    filter(!is.na(value)) %>%
    select(-value) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code) %>%
    pivot_wider(names_from = code, values_from = code) %>%
    set_names(c(id_cols, paste0(var, "_", colnames(.)[(length(id_cols) + 1):ncol(.)])))

  full_decoded_data <- select(input_data, -all_of(var)) %>%
    left_join(decoded_data, by = id_cols)

  return(full_decoded_data)

}
