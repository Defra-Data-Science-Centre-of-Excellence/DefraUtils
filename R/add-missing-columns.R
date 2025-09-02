#' Add missing columns
#'
#' Given a list of columns which should occur in a dataset, adds in the missing
#' columns and fill them with the supplied default value. Will only add columns,
#' not remove them.
#'
#' @importFrom dplyr %>% select if_else
#' @importFrom tibble add_column
#' @importFrom rlang set_names
#'
#' @param df A dataset
#' @param full_column_list A character vector of all the columns which should
#' appear in the dataset
#' @param fill_value The value that the new columns should be populated with
#' @param reorder Logical, default = `TRUE`; Should the columns of the output
#' data be reordered using the `full_column_list`?
#'
#' @export

add_missing_columns <- function(df, full_column_list, fill_value = NA, reorder = TRUE) {

  missing_columns <- full_column_list[!full_column_list %in% names(df)]

  full_df <- df %>%
    add_column(!!!set_names(as.list(rep(fill_value, length(missing_columns))),
                            nm = missing_columns))

  other_columns <- names(full_df)[!names(full_df) %in% full_column_list]

  reordered_data <- select(full_df, all_of(full_column_list))

  if (reorder) { return(reordered_data) } else { return(full_df) }

}
