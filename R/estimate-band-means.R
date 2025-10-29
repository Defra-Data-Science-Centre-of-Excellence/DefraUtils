# Estimate means of banded data for calculating population averages

#' Create bands
#'
#' Creates a tibble of bands with corresponding lower and upper limits and labels.
#'
#' @param start Numeric (default = 0); the start of the range
#' @param upper_limits Numeric (default = `NULL`); the upper limits of each
#' band, only used if `step` is not specified
#' @param open_ended Logical (default = `FALSE`); whether to add a open-ended
#' band as the last band (i.e. include all values greater than the upper limit)
#' @param codes Numeric (default = `NULL`); the codes used in the dataset for each
#' band - if NULL, will use a numeric sequence from 1 to the total number of bands
#' @param prefix String; a prefix to apply to the band values (for example, "£"
#' when the values are in GBP)
#'
#' @return A tibble with columns for the band codes, lower limits, upper limits
#' and band labels
#'
#' @examples
#' create_bands_df(start = 1, upper_limits = c(10, 1000, 10000), prefix = "£")
#' create_bands_df(start = 0, upper_limits = c(10, 1000, 10000), open_ended = TRUE)
#'
#' @importFrom dplyr %>% tibble mutate
#' @importFrom scales label_comma
#'
#' @export
#'
#' @family functions for estimating means of banded data
create_bands_df <- function(start = 0, upper_limits = NULL, open_ended = FALSE,
                            codes = NULL, prefix = "") {


  if (length(upper_limits) == 1) {
    stop("'upper_limits' must have at least two elements.")
  }
  if (min(upper_limits) < start) {
    stop("'upper_limits' must be greater than or equal to 'start'.")
  }

  if (open_ended) {
    # Set the upper limit of the last income band to Inf to make it open-ended
    upper_limits <- c(upper_limits, Inf)
  }

  if (is.null(codes)) {
    codes <- 1:(length(upper_limits))
  }

  bands_df <- tibble(code = codes,
                     lower_limit = c(start, (head(upper_limits, -1)) + 1),
                     upper_limit = upper_limits) %>%
    # Add income band labels
    mutate(band = factor(ifelse(
      upper_limit == Inf,
      paste0(label_comma(prefix = prefix)(lower_limit), " and over"),
      sprintf("%s to %s", label_comma(prefix = prefix)(lower_limit),
              label_comma(prefix = prefix)(upper_limit)))))

  return(bands_df)

}

#' Count the population in each band using weights
#'
#' Used by [estimate_band_means()]
#'
#' @param df A data frame containing banded incomes
#' @param bands_df A table of bands, corresponding with the values in `band_col`,
#' created using [create_bands_df()]
#' @param band_col String; The name of the column in `df` with bands in
#' @param grouping_cols String (default = "year"); Columns to group the counts
#' by, along with `band` (if no groups, set to `NULL`)
#' @param weights String (default = "weight"); The name of the column containing
#' the weights
#'
#' @importFrom rlang sym
#' @importFrom dplyr group_by summarise right_join arrange filter n join_by
#'
#' @export
#'
#' @family functions for estimating means of banded data
compute_band_counts <- function(df, bands_df, band_col,
                                grouping_cols = "year",
                                weights = "weight") {

  band_counts <- df %>%
    # Recode any values below the first lower limit to the first band
    # This is required if negative values are recorded using the negative
    # version of the positive band, like in the FBS
    mutate(across(all_of(band_col), ~if_else(.x < bands_df$lower_limit[1],
                                             bands_df$code[1], .x))) %>%
    # Count the observations by band and grouping
    group_by(across(all_of(c(grouping_cols, band_col)))) %>%
    summarise(n = n(), n_pop = sum(.data[[weights]]), .groups = "drop") %>%
    # Join on the band labels
    right_join(bands_df, by = join_by(!!sym(band_col) == code), multiple = "all") %>%
    arrange(across(all_of(grouping_cols)), upper_limit) %>%
    # Remove any empty bands
    filter(!is.na(n_pop))

  return(band_counts)

}

#' Mean Constrained Integration Over Brackets (MCIB)
#'
#' Used by [estimate_band_means()]. Run to check how MCIB has been calculated.
#'
#' [https://journals.sagepub.com/doi/10.1177/0081175018782579#sec-4](https://journals.sagepub.com/doi/10.1177/0081175018782579#sec-4)
#'
#' @param band_counts_df Counts of bands, created using [compute_band_counts()]
#' @param bands_df A table of bands, created using [create_bands_df()]
#'
#' @importFrom dplyr %>% mutate rowwise ungroup lead lag filter
#' @importFrom tibble rowid_to_column
#'
#' @export
#'
#' @family functions for estimating means of banded data
calculate_mcib <- function(band_counts_df, bands_df) {

  # Only one group of data can be done at a time
  if (!isTRUE(all.equal(unique(band_counts_df$band), band_counts_df$band))) {
    stop("The band column of band_counts_df is not unique; run on one group at a time")
  }

  top_band <- last(bands_df$band)
  n_pop_total <- sum(band_counts_df$n_pop)

  mcib <- band_counts_df %>%
    rowid_to_column(var = "bin") %>%                        # number the bands
    mutate(
      midpoint = (lower_limit + upper_limit) / 2,           # the actual band midpoint
      pct_n = 100 * n_pop / n_pop_total,                    # the % of the population in the band
      cum_n = cumsum(n_pop),                                # cumulative sum of the population
      cum_pct_n = 100 * cum_n / n_pop_total,                # cumulative % of the population
      density = n_pop / (upper_limit - lower_limit),        # density of the band
      slope1 = (density - lag(density)) / (midpoint - lag(midpoint)),   # variance compared to previous band
      slope2 = (lead(density) - density) / (lead(midpoint) - midpoint), # variance compared to next band
      slope1 = ifelse(is.na(slope1), slope2, slope1),       # make sure slope 1 has no NAs
      slope2 = ifelse(is.na(slope2), slope1, slope2),       # make sure slope 2 has no NAs
      slope = (slope1 + slope2) / 2) %>%                    # average of slope 1 and slope 2
    rowwise() %>%
    mutate(
      # the minimum and maximum values the slope could be,
      min_slope = (0 - density)/(upper_limit - midpoint),
      max_slope = density/(midpoint - lower_limit)) %>%
    ungroup() %>%
    mutate(
      # find bands which need a uniform within-bracket distribution
      # and check that slope is not above max or below min values
      final_slope = case_when(bin == 1 ~ 0,
                              density > lag(density) & density > lead(density) ~ 0,
                              density < lag(density) & density < lead(density) ~ 0,
                              slope > max_slope ~ max_slope,
                              slope < min_slope ~ min_slope,
                              TRUE ~ slope),
      # get the coefficient for calculating the means
      coeff = density - final_slope * midpoint,
      # calculate the means and totals
      mean = (1/n_pop) * ((final_slope * upper_limit^3 / 3 + coeff * upper_limit^2 / 2) -
                            (final_slope * lower_limit^3 / 3 + coeff * lower_limit^2 / 2)),
      total = n_pop * mean,
      across(c(mean, total), ~tidyr::replace_na(.x, 0))
    ) %>%
    # remove the last bracket if it's there and open ended
    { if (str_detect(top_band, "and over")) filter(., band != top_band) else . }

  return(mcib)

}

#' Robust Pareto Midpoint Estimator (RPME)
#'
#' Used by [estimate_band_means()]. Run to check how RPME has been calculated.
#'
#' @importFrom dplyr %>% filter slice_tail tibble case_match
#' @importFrom rlang arg_match
#'
#' @param band_counts_df Counts of bands, created using [compute_band_counts()]
#' @param method What kind of average to return: "Arithmetic", "Harmonic",
#' "Geometric" or "Median"
#' @param min_alpha Numeric; the minimum threshold for the alpha parameter of
#' the Pareto distribution
#'
#' @export
#'
#' @family functions for estimating means of banded data
calculate_rpme <- function(band_counts_df,
                           method = c("Geometric", "Arithmetic", "Harmonic", "Median"),
                           min_alpha = 1.11) {

  method <- arg_match(method)

  # This only need to be run on data with an open top band
  if (max(band_counts_df$upper_limit) != Inf) {
    stop("Top band is not open-ended")
  }

  # Only one group of data can be done at a time
  if (!isTRUE(all.equal(unique(band_counts_df$band), band_counts_df$band))) {
    stop("The band column of band_counts_df is not unique; run on one group at a time")
  }

  # We need the top and the next band that has > 0 n_pop to calculate alpha
  # We assume only one group of data has been passed
  top_bands <- filter(band_counts_df, n_pop > 0) %>%
    arrange(upper_limit) %>%
    slice_tail(n = 2)

  # Calculate the alpha estimate
  alpha_estimate <- (log(top_bands$n_pop[1] + top_bands$n_pop[2]) - log(top_bands$n_pop[2])) /
    (log(top_bands$lower_limit[2]) - log(top_bands$lower_limit[1]))

  # Add calculations to data
  rpme <- filter(band_counts_df, upper_limit == Inf) %>%
    mutate(
      # if the alpha estimate is below the min_alpha, replace
      alpha = max(min_alpha, alpha_estimate),
      # calculate each coefficient
      arithmetic_coeff = alpha / (alpha - 1),
      harmonic_coeff = (1 + 1 / alpha),
      geometric_coeff = exp(1/alpha),
      median_coeff = 2^(1/alpha),
      # calculate the mean based on the method passed
      mean = lower_limit * !!sym(paste0(tolower(method), "_coeff")),
      total = mean * n_pop)

  return(rpme)

}

#' Calculate Robust Integration Over Brackets
#'
#' Used by [estimate_band_means()]. You can use this function to check that band
#' labels have been assigned correctly, otherwise use [estimate_band_means()].
#'
#' @importFrom dplyr %>% select bind_rows
#' @importFrom rlang arg_match
#'
#' @param band_counts_df Counts of bands, created using [compute_band_counts()];
#' fed into both [calculate_mcib()] and [calculate_rpme()]
#' @param bands_df A table of bands, created using [create_bands_df()]; fed
#' into [calculate_mcib()]
#' @param method String; one of "Arithmetic", "Harmonic", "Geometric" or
#' "Median", fed into [calculate_rpme()]
#' @param min_alpha Numeric; the minimum threshold for the alpha parameter of
#' the Pareto distribution, fed into [calculate_rpme()]
#'
#' @export
#'
#' @family functions for estimating means of banded data
calculate_riob <- function(band_counts_df, bands_df,
                           method = c("Geometric", "Arithmetic", "Harmonic", "Median"),
                           min_alpha = 1.11) {

  method <- arg_match(method)

  # Only calculate RPME if the open top band is present in the data
  riob <- if (max(band_counts_df$upper_limit) == Inf) {

    lower_band_estimates <- calculate_mcib(band_counts_df = band_counts_df,
                                           bands_df = bands_df) %>%
      select(c(colnames(band_counts_df), "total", "mean"))

    top_band_estimate <- calculate_rpme(band_counts_df = band_counts_df,
                                        method = method, min_alpha = min_alpha) %>%
      select(c(colnames(band_counts_df), "total", "mean"))

    bind_rows(lower_band_estimates, top_band_estimate)

  } else {

    calculate_mcib(band_counts_df = band_counts_df, bands_df = bands_df) %>%
      select(c(colnames(band_counts_df), "total", "mean"))

  }

  return(riob)

}

#' Apply Robust Integration Over Brackets
#'
#' Combines Mean Constrained Integration Over Brackets and Robust Pareto
#' Midpoint Estimator to estimate the mean value of each band, in banded data.
#' Can be done by groups, e.g. year, or year and region, using the
#' `grouping_cols` argument.
#'
#' Mean Constrained Integration Over Brackets:
#' [https://journals.sagepub.com/doi/10.1177/0081175018782579#sec-4](https://journals.sagepub.com/doi/10.1177/0081175018782579#sec-4)
#'
#' We don't have a grand mean, so we only do the integration step.
#'
#' Robust pareto midpoint estimator:
#' [https://ideas.repec.org/c/boc/bocode/s457962.html](https://ideas.repec.org/c/boc/bocode/s457962.html)
#' [https://arxiv.org/ftp/arxiv/papers/1402/1402.4061.pdf](https://arxiv.org/ftp/arxiv/papers/1402/1402.4061.pdf)
#'
#' The default method is geometric and the default min_alpha value is 1.11,
#' which are the ideal options for the FBS.
#' These were chosen by calculating the Pareto Distribution using min_alpha
#' values from 0-4 in 0.01 increments for each of the four mean methods. The
#' method with the lowest errors was chosen as the default and of that method,
#' the alpha value with an error value closest to zero was chosen as the default
#' min_alpha value.
#' This will need to be updated if you are not using FBS data, or the FBS sample
#' size has significantly changed.
#'
#' @importFrom rlang arg_match sym
#' @importFrom dplyr %>% last group_by group_split add_row
#' @importFrom tidyr replace_na fill
#'
#' @param df A data frame containing the survey data
#' @param bands_df A table of bands, corresponding with the values in `band_col`,
#' created using [create_bands_df()]; fed into [compute_band_counts()], and
#' [calculate_mcib()] via [calculate_riob()]
#' @param band_col String; The name of the column in `df` with bands in, fed
#' into [compute_band_counts()]
#' @param grouping_cols String (default = "year"); Columns to group the counts
#' by, along with `band`, fed into [compute_band_counts()] (if no groups, set
#' to `NULL`)
#' @param weights String (default = "weight"); The name of the column containing
#' the weights, fed into [compute_band_counts()]
#' @param method String; one of "Arithmetic", "Harmonic", "Geometric" or
#' "Median", fed into [calculate_rpme()] via [calculate_riob()]
#' @param min_alpha Numeric; the minimum threshold for the alpha parameter of
#' the Pareto distribution, fed into [calculate_rpme()] via [calculate_riob()]
#'
#' @export
#'
#' @family functions for estimating means of banded data
estimate_band_means <- function(df, bands_df, band_col,
                                grouping_cols = "year", weights = "weight",
                                method = c("Geometric", "Arithmetic", "Harmonic", "Median"),
                                min_alpha = 1.11) {

  method <- arg_match(method)

  grouped_band_counts <- df %>%
    compute_band_counts(bands_df = bands_df, weights = weights,
                        band_col = band_col, grouping_cols = grouping_cols) %>%
    group_by(across(all_of(grouping_cols))) %>%
    group_split()

  band_estimates <- lapply(grouped_band_counts, \(df_x) {
    calculate_riob(df_x, bands_df = bands_df, method = method, min_alpha = min_alpha) %>%
      add_row(n = sum(.$n),
              n_pop = sum(.$n_pop),
              lower_limit = min(.$lower_limit),
              upper_limit = max(.$upper_limit),
              band = "All",
              total = sum(.$total),
              mean = total / n_pop) %>%
      fill(all_of(grouping_cols))
  }) %>% bind_rows() %>%
    select(-all_of(band_col)) %>%
    rename_with(~band_col, band)

  return(band_estimates)

}
