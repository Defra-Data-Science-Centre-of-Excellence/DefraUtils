#' Fix suppression in tables where totals can be used to calculate suppressed values
#'
#' This function finds which figures need to be secondarily suppress and
#' overwrites their sample size to zero - any subsequent suppression code will
#' then suppress these figures.
#'
#' @importFrom dplyr %>% mutate group_by ungroup summarise bind_rows distinct
#' left_join join_by arrange filter lag select
#' @importFrom rlang arg_match
#'
#' @param df Data to fix
#' @param suppression_type Which table you are fixing
#'
#' @export

fix_suppression <- function(df, suppression_type = c(
  "mean_incomes", "outputs_costs", "details", "crop_stock_labour",
  "income_distributions", "balance_sheets", "liabilities", "total_diversified",
  "fbi_diversification", "diversified_outputs_incomes",
  "diversification_output_dist", "agricultural_transition",
  "ofi_median", "ofi_proportion", "machinery", "custom"),
  suppression_level = c("values", "sample_size"),
  sample_size_col = "sample_size", custom_groups = NULL) {

  suppression_type <- rlang::arg_match(suppression_type)
  suppression_level <- rlang::arg_match(suppression_level)

  rows_to_check <- if (suppression_type %in% c("crop_stock_labour", "balance_sheets")) {
    filter(df, word(measure, 1) == "Number")
  } else if (suppression_type == "agricultural_transition") {
    filter(df, fbs_variable %in% c("retirement.plans_option",
                                   "management.control.changes_option",
                                   "sfi.land.changes_option"))
  } else if (suppression_type == "machinery" & suppression_level == "values") {
    filter(df, measure == "Percentage of grant-using farms")
  } else if (suppression_type == "machinery" & suppression_level == "sample_size") {
    filter(df, str_detect(measure, "Average.*grant"))
  } else { df }

  # Find which values should be suppressed initially
  initial_suppression <- rows_to_check %>%
    mutate(suppress = between(!! sym(sample_size_col), 1, 4))

  # Group the dataset correctly
  groupings <- if (suppression_type %in% c("mean_incomes", "outputs_costs", "details", "crop_stock_labour")) {
    c("fbs_year", "farm_type")
  } else if (suppression_type == "income_distributions") {
    c("farm_type", "fbs_variable")
  } else if (suppression_type == "balance_sheets") {
    c("farm_type", "region", "opening_closing")
  } else if (suppression_type == "liabilities") {
    c("farm_type", "tenure_type", "fbs_variable")
  } else if (suppression_type == "fbi_diversification") {
    c("survey_year")
  } else if (suppression_type %in% c("diversified_outputs_incomes")) {
    c("measure", "diversification_type")
  } else if (suppression_type %in% c("diversification_output_dist")) {
    c("measure")
  } else if (suppression_type == "agricultural_transition") {
    c("survey_year", "fbs_variable")
  } else if (suppression_type == "ofi_median") {
    c("fbs_year", "measure", "grouping_factor")
  } else if (suppression_type == "ofi_proportion") {
    c("fbs_year", "measure", "grouping_factor")
  } else if (suppression_type == "machinery") {
    c("survey_year", "typology", "prices")
  } else if (suppression_type == "custom") {
    custom_groups
  }

  grouped_data <- group_by(initial_suppression, across(all_of(groupings)))

  # Get the number of suppressed values in each group
  count_suppressed <- grouped_data %>%
    summarise(n = sum(suppress), .groups = "keep") %>%
    mutate(supp_type = "n_obs")

  # Get the total of the suppressed values in each group
  total_suppressed <- filter(grouped_data, suppress) %>%
    summarise(n = sum(!! sym(sample_size_col)), .groups = "keep") %>%
    mutate(supp_type = "total")

  to_fix <- bind_rows(filter(count_suppressed, n == 1),
                      filter(total_suppressed, n < 5)) %>%
    distinct(across(1:(ncol(.) - 2)), .keep_all = T)

  # If there is only one suppressed value in a group, or all of the
  # suppressed values do not add up to at least 5, also suppress the
  # value in the group with the next fewest farms
  # This is done by re-assigning the sample size column value to be 1 so that
  # the rounding and suppression code later on will suppress it
  fixed <- if (nrow(to_fix) > 0) {

    fix_col <- left_join(to_fix, rows_to_check, by = groupings) %>%
      group_by(across(all_of(groupings))) %>%
      arrange(!! sym(sample_size_col), .by_group = TRUE) %>%
      # We want to show true zeroes
      filter(!! sym(sample_size_col) != 0) %>%
      mutate(sample_size_2 = if_else(
        # If it's the fewest or next fewest number of farms, return 1
        lag(!! sym(sample_size_col)) < 5 | is.na(lag(!! sym(sample_size_col))), 1,
        # Otherwise return the original value
        !! sym(sample_size_col))) %>%
      ungroup() %>%
      select(-c(n, supp_type))

    # Join on the new sample size column and replace the original
    # values where relevant
    df %>%
      left_join(fix_col, by = colnames(df)) %>%
      mutate(sample_size_1 = if_else(is.na(sample_size_2),
                                     !! sym(sample_size_col),
                                     sample_size_2)) %>%
      select(-all_of(c(sample_size_col, "sample_size_2"))) %>%
      rename({{sample_size_col}} := sample_size_1) %>%
      select(all_of(colnames(df)))

    # If the table does not need fixing then return the original df
  } else { df }

  return(fixed)

}
