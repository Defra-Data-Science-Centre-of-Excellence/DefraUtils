# Testing band dataframe creation ####
test_that("start and upper_limits arguments work", {
  start_end_df <- create_bands_df(start = 0, upper_limits = c(10, 20, 30))
  expect_equal(start_end_df$lower_limit, c(0, 11, 21))
  expect_equal(start_end_df$upper_limit, c(10, 20, 30))
})

test_that("open_ended argument works", {
  open_ended_df <- create_bands_df(start = 0, upper_limits = c(10, 20, 30), open_ended = TRUE)
  expect_equal(open_ended_df$lower_limit, c(0, 11, 21, 31))
  expect_equal(open_ended_df$upper_limit, c(10, 20, 30, Inf))
})

test_that("codes argument works", {
  expect_equal(create_bands_df(start = 0, upper_limits = c(10, 20, 30))$code, 1:3)
  expect_equal(create_bands_df(start = 0, upper_limits = c(10, 20, 30),
                               codes = c(2, 4, 6))$code, c(2, 4, 6))
})

test_that("band column with prefix argument works", {
  expect_true(all(stringr::str_detect(
    as.character(create_bands_df(start = 0, upper_limits = c(10, 20, 30))$band),
    "^\\d+ to \\d+$")))
  expect_true(all(stringr::str_detect(
    as.character(create_bands_df(start = 0, upper_limits = c(10, 20, 30),
                                 prefix = "£")$band), "^£\\d+ to £\\d+$")))
})

# Testing band counts ####
test_df <- dplyr::tibble(
  id = rep(1:10, 2),
  year = c(rep(2020, 10), rep(2025, 10)),
  weight = rep(c(2.4, 4.6, 4.6, 1.5, 2.8, 2.8, 1.4, 4.2, 4.3, 1.6), 2),
  question = c(2, 2, 3, 1, 3, 3, 1, 1, 2, -2,
               3, 3, 1, 2, 2, 1, 1, 3, 2, -1)
  )

bands_df_closed <- create_bands_df(start = 0, upper_limits = c(10, 20, 30))
bands_df_open <- create_bands_df(start = 0, upper_limits = c(10, 20), open_ended = T)

test_that("expected columns are returned", {
  expect_named(compute_band_counts(test_df, bands_df = bands_df_closed, weights = "weight",
                                   band_col = "question", grouping_cols = "year"),
               c("year", "question", "n", "n_pop", "lower_limit", "upper_limit", "band"))
})

test_that("band counts are calulated correctly", {
  expect_equal(compute_band_counts(left_join(test_df, bands_df_closed, by = c("question" = "code")),
                                   bands_df = bands_df_closed, weights = "weight",
                                   band_col = "question", grouping_cols = "year") %>%
                 dplyr::pull(n_pop),
               mapply(\(x, y) test_df %>%
                        dplyr::mutate(question = if_else(question < 0, 1, question)) %>%
                        dplyr::filter(year == x, question == y) %>%
                        dplyr::pull(weight) %>% sum(),
                      c(rep(2020, 3), rep(2025, 3)), rep(1:3, 2)))
})

# Testing band estimates ####
band_counts_closed <- compute_band_counts(test_df, bands_df_closed, weights = "weight",
                                          band_col = "question", grouping_cols = "year")

band_counts_open <- compute_band_counts(test_df, bands_df_open, weights = "weight",
                                        band_col = "question", grouping_cols = "year")

## Testing MCIB ####
test_that("multiple groups returns error", {
  expect_error(calculate_mcib(band_counts_closed, bands_df_closed),
               "The band column of band_counts_df is not unique; run on one group at a time")
})

test_that("expected columns are returned", {
  expect_named(calculate_mcib(dplyr::filter(band_counts_closed, year == 2020), bands_df_closed),
               c("bin", "year", "question", "n", "n_pop", "lower_limit",
                 "upper_limit", "band", "midpoint", "pct_n", "cum_n", "cum_pct_n",
                 "density", "slope1", "slope2", "slope", "min_slope", "max_slope",
                 "final_slope", "coeff", "mean", "total"))
})

test_that("open top band is not returned", {
  expect_equal(unique(calculate_mcib(dplyr::filter(band_counts_open, year == 2020),
                                     bands_df_open)$question), 1:2)
})

test_that("band means are within limits", {
  expect_true(calculate_mcib(dplyr::filter(band_counts_closed, year == 2020), bands_df_closed) %>%
                dplyr::select(bin, year, question, lower_limit, upper_limit, mean) %>%
                dplyr::mutate(mean_check = dplyr::between(mean, lower_limit, upper_limit)) %>%
                dplyr::pull(mean_check) %>% all())
  expect_true(calculate_mcib(dplyr::filter(band_counts_open, year == 2020), bands_df_open) %>%
                dplyr::select(bin, year, question, lower_limit, upper_limit, mean) %>%
                dplyr::mutate(mean_check = dplyr::between(mean, lower_limit, upper_limit)) %>%
                dplyr::pull(mean_check) %>% all())
})

## Testing RPME ####
test_that("closed top band returns error", {
  expect_error(calculate_rpme(band_counts_closed), "Top band is not open-ended")
})

test_that("multiple groups returns error", {
  expect_error(calculate_rpme(band_counts_open),
               "The band column of band_counts_df is not unique; run on one group at a time")
})

test_that("expected columns are returned", {
  expect_named(calculate_rpme(dplyr::filter(band_counts_open, year == 2020)),
               c("year", "question", "n", "n_pop", "lower_limit", "upper_limit",
                 "band", "alpha", "arithmetic_coeff", "harmonic_coeff",
                 "geometric_coeff", "median_coeff", "mean", "total"))
})

test_that("only open top band is returned", {
  expect_equal(calculate_rpme(dplyr::filter(band_counts_open, year == 2020))$question, 3)
})

test_that("band mean is above lower limit", {
  expect_true(calculate_rpme(filter(band_counts_open, year == 2020)) %>%
                dplyr::select(lower_limit, mean) %>%
                dplyr::mutate(mean_check = mean > lower_limit) %>%
                dplyr::pull(mean_check))
})


## Testing RIOB ####
test_that("expected data is returned", {
  expect_named(calculate_riob(filter(band_counts_closed, year == 2020), bands_df_closed),
               c("year", "question", "n", "n_pop", "lower_limit",
                 "upper_limit", "band", "total", "mean"))
  expect_named(calculate_riob(filter(band_counts_open, year == 2020), bands_df_open),
               c("year", "question", "n", "n_pop", "lower_limit",
                 "upper_limit", "band", "total", "mean"))
})

test_that("open top band results in different estimate", {
  expect_false(dplyr::last(calculate_riob(filter(band_counts_closed, year == 2020), bands_df_closed)$mean) ==
                 dplyr::last(calculate_riob(filter(band_counts_open, year == 2020), bands_df_open)$mean))
})


## Testing all estimates ####
test_that("expected data is returned", {
  expect_named(estimate_band_means(test_df, bands_df_closed, "question"),
               c("year", "n", "n_pop", "lower_limit",
                 "upper_limit", "question", "total", "mean"))
  expect_named(estimate_band_means(test_df, bands_df_closed, "question", grouping_cols = NULL),
               c("n", "n_pop", "lower_limit", "upper_limit", "question", "total", "mean"))
})

test_that("'all' rows have been calculated correctly", {
  expect_equal(estimate_band_means(test_df, bands_df_closed, "question") %>%
                 dplyr::filter(question != "All") %>%
                 dplyr::group_by(year) %>%
                 dplyr::summarise(question = "All",
                           n_pop = sum(n_pop),
                           total = sum(total),
                           mean = total / n_pop),
               estimate_band_means(test_df, bands_df_closed, "question") %>%
                 dplyr::filter(question == "All") %>%
                 dplyr::select(year, question, n_pop, total, mean))
})
