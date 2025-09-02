#' Round numbers in the FBS style
#'
#' The FBS style of rounding is show 0 as 0, rounding everything >= 100 to the
#' nearest 100, everything else that is >= 10 to the nearest 1, and everything
#' else to 1 decimal place
#'
#' For areas and gross margins, rounding is slightly different: we show 0 as 0,
#' round everything else that is >= 10 to the nearest 1 and everything else to
#' 1 decimal place
#'
#' We can also use this function to round to any power of 10
#'
#' Note that the janitor round_half_up function is used, as opposed to R's
#' default 'round-to-even' method
#'
#' @importFrom scales comma
#' @importFrom janitor round_half_up
#' @importFrom rlang arg_match
#'
#' @param x The number to round
#' @param round_to numeric, default = NULL; if NULL, will default to the FBS
#' style, alternatively, provide any power of 10 (including minus powers, e.g. 0.1)
#' @param value_type If "area_gm" is supplied, the different method of rounding
#' (explained above) will be used
#'
#' @return A rounded number in string format
#'
#' @export

fbs_round <- function(x, round_to = NULL, value_type = c("", "area_gm")) {

  value_type <- rlang::arg_match(value_type)

  if (is.null(round_to)) {

    rounded <- if (value_type == "area_gm") {

      # Show 0 as 0, round everything else that is >= 10 to the nearest 1 and
      # everything else to 1 decimal place
      comma(
        case_when(x > -0.05 & x < 0.05 ~ 0,
                  x <= -10 | x >= 10 ~ round_half_up(x, 0),
                  TRUE ~ round_half_up(x, 1)),
        accuracy = ifelse(is.na(x) | (x > -0.05 & x < 0.05) | x <= -10 | x >= 10,
                          1, 0.1))

    } else {

      # Show 0 as 0, round everything >= 100 to the nearest 100, everything else
      # that is >= 10 to the nearest 1, and everything else to 1 decimal place
      comma(case_when(
        x > -0.05 & x < 0.05 ~ 0,
        x <= -100 | x >= 100 ~ round_half_up(x, -2),
        x <= -10 | x >= 10 ~ round_half_up(x, 0),
        TRUE ~ round_half_up(x, 1)),
        accuracy = ifelse(is.na(x) | (x > -0.05 & x < 0.05) | x <= -10 | x >= 10,
                          1, 0.1))

    }

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
      accuracy = if_else(round_to > 0 & round_to < 1, round_to, 1))

  }

  return(rounded)

}
