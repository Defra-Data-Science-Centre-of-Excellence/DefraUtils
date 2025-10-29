#' Round numbers for publishing/sharing
#'
#' Round values with [janitor::round_half_up()] (as opposed to the R default
#' 'round-to-even' method) and add comma separators.
#'
#' 'Optimise' rounds smaller numbers to more detail and larger numbers to less
#' detail.
#'
#' When `optimise_to` is set to `"1"`:
#' * 0 is shown as 0
#' * Values > 0 and < 10 are rounded to 1 decimal place
#' * Values >= 10 are rounded to the nearest 1
#'
#' When `optimise_to` is set to `"100"` the above plus:
#' * Values >= 10 and < 100 are rounded to the nearest 1
#' * Values >= 100 are rounded to the nearest 100
#'
#' When `optimise_to` is set to `"1k"` (i.e. 1,000) the above plus:
#' * Values >= 100 and < 1,000 are rounded to the nearest 100
#' * Values >= 1,000 are rounded to the nearest 1,000
#'
#' And so on, for `"10k"` (10,000), `"100k"` (100,000), `"1m"` (1,000,000),
#' `"10m"` (10,000,000), and `"100m"` (100,000,000),
#'
#' @importFrom dplyr case_when
#' @importFrom scales comma
#' @importFrom janitor round_half_up
#' @importFrom rlang arg_match
#'
#' @param x The number to round
#' @param method Either `"round_to"` for rounding all numbers to a power of 10
#' (must supply `round_to` argument), or `"optimise"` for rounding smaller
#' numbers to more detail and larger numbers to less detail (see details)
#' @param optimise_to The maximum value values are rounded to, as a string
#' (1, or powers of 10 from 100 to 100 million, see details)
#' @param round_to numeric; if method is set to `"round_to"`, provide any power
#' of 10 (including minus powers, e.g. 0.1)
#' @param round_zeros When `method = "optimise"`, should zeros be shown with no
#' decimal places (`TRUE`), or to 1 decimal place (`FALSE`)?
#' @param ... Optional arguments to [scales::comma()] (other than `accuracy`,
#' which this function takes care of)
#'
#' @return A rounded value with comma separators, plus optional prefix and suffix
#'
#' @export

round_with_commas <- function(x, method = c("optimise", "round_to"),
                              optimise_to = c("1", "100", "1k", "10k",
                                              "100k", "1m", "10m", "100m"),
                              round_to = NULL, round_zeros = TRUE, ...) {

  method <- arg_match(method)
  optimise_to <- arg_match(optimise_to)

  if (method == "optimise") {

    optimise_args <- c("1", "100", "1k", "10k", "100k", "1m", "10m", "100m")

    rounded <- comma(
      case_when(x > -0.05 & x < 0.05 ~ 0,
                !optimise_to %in% optimise_args[1:7] & (x <= -1e8 | x >= 1e8) ~ round_half_up(x, -8),
                !optimise_to %in% optimise_args[1:6] & (x <= -1e7 | x >= 1e7) ~ round_half_up(x, -7),
                !optimise_to %in% optimise_args[1:5] & (x <= -1e6 | x >= 1e6) ~ round_half_up(x, -6),
                !optimise_to %in% optimise_args[1:4] & (x <= -1e5 | x >= 1e5) ~ round_half_up(x, -5),
                !optimise_to %in% optimise_args[1:3] & (x <= -1e4 | x >= 1e4) ~ round_half_up(x, -4),
                !optimise_to %in% optimise_args[1:2] & (x <= -1e3 | x >= 1e3) ~ round_half_up(x, -3),
                !optimise_to %in% optimise_args[1]   & (x <= -100 | x >= 100) ~ round_half_up(x, -2),
                x <= -10 | x >= 10 ~ round_half_up(x, 0),
                TRUE ~ round_half_up(x, 1)),
      accuracy = ifelse(is.na(x) | (x > -0.05 & x < 0.05 & round_zeros) | x <= -10 | x >= 10,
                        1, 0.1),
      ...)

  } else {

    # Get the digits for round_half_up from the round_to argument
    digits <- if (round_to > 0 & round_to < 1) {
      nchar(as.character(1/round_to)) - 1
    } else if ((1/round_to) %% 1 == 0) {
      0
    } else {
      0 - nchar(strsplit(sub("0+$", "", as.character(1/round_to)),
                         ".", fixed = TRUE)[[1]][[2]])
    }

    rounded <- comma(
      round_half_up(x, digits),
      accuracy = if_else(round_to > 0 & round_to < 1, round_to, 1),
      ...)

  }

  return(rounded)

}
