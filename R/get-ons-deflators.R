#' Read in ONS deflator data and run real terms calculation
#'
#' This mimics what is done for the Oct/Nov income publication
#' Works on the inbuilt GDP and CPIH data, which is downloaded from ONS
#'
#' @importFrom dplyr %>% mutate filter pull
#' @importFrom readr read_csv
#' @importFrom stringr str_detect
#' @importFrom zoo as.yearqtr rollmean
#'
#' @param deflator_years The years to get the ONS deflators for
#' @param index The index to use; either GDP or CPIH
#'
#' @return data.frame; The ONS deflators for all years in `deflator_years`
#'
#' @export

get_gdp_deflators <- function(deflator_years, index = c("GDP", "CPIH")) {

  index <- rlang::arg_match(index)

  if (as.character(match.call()[[1]]) == "get_gdp_deflators") {
    warning("Please use get_ons_deflators() instead of get_gdp_deflators()", call. = FALSE)
  }

  deflator_data <- if (index == "GDP") {
    gdp_deflators
  } else if (index == "CPIH") {
    cpih_deflators
  } else {
    stop("Index should be either GDP or CPIH")
  }

  full_deflator_data <- deflator_data %>%
    setNames(c("yearqtr", "index")) %>%
    mutate(index = suppressWarnings(as.numeric(index))) %>%
    # Filter for data by quarter
    filter(!is.na(index), str_detect(yearqtr, "Q")) %>%
    # Add year and quarter columns and calculate the rolling mean
    mutate(yearqtr = as.yearqtr(yearqtr),
           year = as.numeric(format(yearqtr, "%Y")),
           qtr = as.numeric(format(yearqtr, "%q")),
           index_year = rollmean(index, 4, fill = NA, align = "right"))

  # Get the deflator - i.e. the rolling annual mean - for the publication year
  # E.g. for 2021/22 this would be 2022 Q1, aka the deflator from March 2022
  defl_latest <- filter(full_deflator_data, year == max(deflator_years) + 1, qtr == 1) %>%
    pull(index_year)

  # Get the deflator for all years in the publication
  adjust_deflator_data <- full_deflator_data %>%
    mutate(
      # Use the deflator from the publication year year as the base
      index_year_real = index_year / defl_latest * 100,
      # Align with the FBS survey years
      fbs_year = year - 1) %>%
    # Filter for only the years we are interested in
    filter(fbs_year >= min(deflator_years), fbs_year <= (max(deflator_years)), qtr == 1) %>%
    select(year = fbs_year, index_year_real)

  return(adjust_deflator_data)

}

#' @export
#' @rdname get_gdp_deflators
get_ons_deflators <- get_gdp_deflators
