test_df <- dplyr::tribble(
  ~prices,   ~survey_year, ~typology, ~grouping, ~group, ~value,
  "Current", 2018,         2015,      "letters", "A",    10,
  "Current", 2018,         2015,      "letters", "B",    15,
  "Current", 2018,         2015,      "letters", "C",    20,
  "Real",    2018,         2015,      "letters", "A",    12,
  "Real",    2018,         2015,      "letters", "B",    18,
  "Real",    2018,         2015,      "letters", "C",    25,
  "Current", 2018,         2017,      "letters", "A",    11,
  "Current", 2018,         2017,      "letters", "B",    17,
  "Current", 2018,         2017,      "letters", "C",    28,
  "Real",    2018,         2017,      "letters", "A",    14,
  "Real",    2018,         2017,      "letters", "B",    20,
  "Real",    2018,         2017,      "letters", "C",    33,
  "Current", 2023,         2017,      "letters", "A",    12,
  "Current", 2023,         2017,      "letters", "B",    10,
  "Current", 2023,         2017,      "letters", "C",    30,
  "Real",    2023,         2017,      "letters", "A",    15,
  "Real",    2023,         2017,      "letters", "B",    12,
  "Real",    2023,         2017,      "letters", "C",    37
)

test_that("the simplest possible dataset returns the correct result", {
  simple_result <- filter(test_df, prices == "Current", typology == 2017, group == "A") %>%
    select(survey_year, value) %>%
    compare_two_years(2018, 2023)

  expect_equal(simple_result$diff, 1)
  expect_equal(simple_result$diff_pc, (12 - 11) / 11)
  expect_equal(simple_result$diff_pc_rnd, "9%")

  expect_equal(simple_result$prev, 11)
  expect_equal(simple_result$prev_rnd, "11")
  expect_equal(simple_result$prev_rnd_t, "0 thousand")
  expect_equal(simple_result$prev_rnd_m, "0 million")

  expect_equal(simple_result$curr, 12)
  expect_equal(simple_result$curr_rnd, "12")
  expect_equal(simple_result$curr_rnd_t, "0 thousand")
  expect_equal(simple_result$curr_rnd_m, "0 million")
})

test_that("points change returns the correct result", {
  points_result <- filter(test_df, prices == "Current", typology == 2017, group == "A") %>%
    select(survey_year, value) %>%
    mutate(value = value / 100) %>%
    compare_two_years(2018, 2023, diff = "points")

  expect_equal(points_result$diff, .01)
  expect_equal(points_result$diff_rnd, "1 percentage point")

  expect_equal(points_result$prev, .11)
  expect_equal(points_result$prev_rnd, "11%")

  expect_equal(points_result$curr, .12)
  expect_equal(points_result$curr_rnd, "12%")
})

test_that("prefix is added correctly", {
  prefix_result <- filter(test_df, prices == "Current", typology == 2017, group == "A") %>%
    select(survey_year, value) %>%
    compare_two_years(2018, 2023, prefix = "£")

  expect_equal(prefix_result$prev_rnd, "£11")
  expect_equal(prefix_result$prev_rnd_t, "£0 thousand")
  expect_equal(prefix_result$prev_rnd_m, "£0 million")

  expect_equal(prefix_result$curr_rnd, "£12")
  expect_equal(prefix_result$curr_rnd_t, "£0 thousand")
  expect_equal(prefix_result$curr_rnd_m, "£0 million")
})

test_that("rounded is done correctly", {
  rounding_result <- filter(test_df, prices == "Current", typology == 2017, group == "A") %>%
    select(survey_year, value) %>%
    mutate(value = value + .2) %>%
    compare_two_years(2018, 2023, method = "round_to", round_to = 0.1)

  expect_equal(rounding_result$prev_rnd, "11.2")
  expect_equal(rounding_result$curr_rnd, "12.2")
})

test_that("thousands are calculated correctly", {
  thousands_result <- filter(test_df, prices == "Current", typology == 2017, group == "A") %>%
    select(survey_year, value) %>%
    mutate(value = value * 1.01e3) %>%
    compare_two_years(2018, 2023)

  expect_equal(thousands_result$prev_rnd_t, "11.1 thousand")
  expect_equal(thousands_result$curr_rnd_t, "12.1 thousand")
})

test_that("millions are calculated correctly", {
  millions_result <- filter(test_df, prices == "Current", typology == 2017, group == "A") %>%
    select(survey_year, value) %>%
    mutate(value = value * 1.01e6) %>%
    compare_two_years(2018, 2023)

  expect_equal(millions_result$prev_rnd_m, "11.1 million")
  expect_equal(millions_result$curr_rnd_m, "12.1 million")
})

test_that("prices are filtered correctly", {
  prices_result <- filter(test_df, typology == 2017, group == "A") %>%
    select(survey_year, prices, value) %>%
    compare_two_years(2018, 2023, price_type = "Real")

  expect_equal(prices_result$prev, 14)
  expect_equal(prices_result$curr, 15)
  expect_equal(prices_result$diff, 1)
})

test_that("typology is filtered correctly", {
  typology_result <- filter(test_df, prices == "Current", group == "A") %>%
    select(survey_year, typology, value) %>%
    compare_two_years(2018, 2023)

  expect_equal(typology_result$prev, 11)
  expect_equal(typology_result$curr, 12)
  expect_equal(typology_result$diff, 1)
})

test_that("groups are calculated correctly", {
  group_result <- filter(test_df, prices == "Current", typology == 2017) %>%
    select(survey_year, group, value) %>%
    compare_two_years(2018, 2023)

  expect_equal(group_result$group, LETTERS[1:3])
  expect_equal(group_result$prev, c(11, 17, 28))
  expect_equal(group_result$curr, c(12, 10, 30))
  expect_equal(group_result$diff, c(1, -7, 2))
})

test_that("groups are renamed correctly", {
  rename_result <- filter(test_df, prices == "Current", typology == 2017) %>%
    select(survey_year, group, value) %>%
    compare_two_years(2018, 2023, group_levels = LETTERS[1:3], group_labels = letters[1:3])

  expect_equal(rename_result$group, LETTERS[1:3])
  expect_equal(rename_result$group_text, factor(letters[1:3]))
})

test_that("extra variables are included correctly", {
  extra_result <- filter(test_df, prices == "Current", typology == 2017) %>%
    select(survey_year, extra = group, value) %>%
    compare_two_years(2018, 2023, extra_vars = "extra")

  expect_equal(extra_result$extra, LETTERS[1:3])
})
