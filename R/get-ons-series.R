#' Read in quarterly ONS price indices and convert to annual series
#'
#' Retrieves an ONS inflation / price indices series. The URLs for the GDP and
#' CPIH series are included, or you can specify your own. Once the series has
#' been retrieved, you can convert a current terms value into real terms
#' using `value / index * 100`, where `index` is the price / deflator index for
#' the year the value corresponds to (see examples).
#'
#' As an example, for the 'CPI ANNUAL RATE 00: ALL ITEMS 2015=100' series, the
#' following URL options will work:
#' * https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7g7/mm23
#' * economy/inflationandpriceindices/timeseries/d7g7/mm23
#' * https://www.ons.gov.uk/generator?format=csv&uri=economy/inflationandpriceindices/timeseries/d7g7/mm23
#'
#' @importFrom dplyr %>% pull filter mutate pull case_when
#' @importFrom readr read_csv parse_number
#' @importFrom stringr str_detect str_replace_all str_remove
#' @importFrom zoo as.yearqtr rollmean
#' @importFrom rlang arg_match
#' @importFrom polite bow nod
#'
#' @param series_years The years to get the ONS indices for (for financial
#' years, just the first year, e.g. 2020 for 2020/21)
#' @param index The index to use; either GDP or CPIH, or leave blank to download
#' another series (must provide a URL)
#' @param ons_url If provided, will download the CSV file at this URL to create
#' the indices; can be the full link to the inflation / price indices series,
#' just the URL extension, or the CSV generator (see details section)
#' @param save_path If provided, will save the downloaded data to the specified
#' directory
#' @param year_end_q When converting the index from quarterly to annual, the
#' year-end to use, i.e. the last quarter of the year; default is 4, which
#' converts to calendar years (for financial years, set as 1)
#'
#' @return A tibble containing the ONS inflation / price indices series for all
#' years in `series_years` (for clarity, a column  of type `yearqtr` showing the
#' chosen year end is also included)
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#'
#' current_terms <- tibble(
#'   year = 2015:2020,
#'   prices = "Current",
#'   value = runif(n = 6, min = 100, max = 1000)
#' )
#'
#' gdp_series <- get_ons_series(2015:2020, index = "GDP")
#'
#' real_terms <- left_join(current_terms, gdp_series,
#'                         by = "year") %>%
#'   mutate(prices = "Real",
#'          value = value / index * 100)
#'
#' bind_rows(current_terms, real_terms)
#'
#' (current_terms$value / real_terms$index) * 100
#'
#' @export

get_ons_series <- function(series_years, index = c("", "GDP", "CPIH"),
                           ons_url = NULL, save_path = NULL, year_end_q = 4) {

  index <- arg_match(index)

  # Get the URL of the data
  series_url <- if (index == "" & is.null(ons_url)) {
    stop("Please provide either index or URL")
  } else if (index == "GDP") {
    # Gross domestic product at market prices:Implied deflator:SA UKEA series
    "https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ybgb/ukea"
  } else if (index == "CPIH") {
    # CPIH INDEX 00: ALL ITEMS 2015=100
    "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
  } else {
    if (str_detect(ons_url, "generator")) {
      ons_url
    } else if (str_detect(ons_url, "www\\.ons\\.gov\\.uk")) {
      str_replace(ons_url, "gov\\.uk", "gov.uk/generator?format=csv&uri=")
    } else {
      paste0("https://www.ons.gov.uk/generator?format=csv&uri=/", ons_url)
    }
  }

  # Introduce yourself to the host
  bow("https://www.ons.gov.uk") %>%
    nod(series_url, verbose = T)

  # Download the file and read in
  temp_destination <- tempfile()
  download.file(series_url, temp_destination)

  series_data <- read_csv(temp_destination, show_col_types = FALSE)

  if (!is.null(save_path)) {
    file_name <- paste0(
      "ons_series_",
      str_replace_all(str_remove(series_url, "h.*timeseries\\/"), "\\/", "_"),
      ".csv")
    write.csv(series_data, file.path(save_path, file_name), row.names = T)
    message(paste("File downloaded to", file.path(save_path, file_name)))
  }

  # Check that the link still works - today's date should be before the next release date
  next_release <- as.Date(pull(filter(series_data, Title == "Next release"), 2), "%d %B %Y")

  if (Sys.Date() > next_release) {
    warning("Next release is showing as in the past, check downloaded data and URL")
  }

  # Get rolling annual means
  full_series_data <- series_data %>%
    setNames(c("yearqtr", "index")) %>%
    mutate(index = suppressWarnings(parse_number(index))) %>%
    # Filter for data by quarter
    filter(!is.na(index), str_detect(yearqtr, "Q")) %>%
    # Add year and quarter columns and calculate the rolling mean
    mutate(yearqtr = as.yearqtr(yearqtr),
           year = as.numeric(format(yearqtr, "%Y")),
           qtr = as.numeric(format(yearqtr, "%q")),
           year_index = rollmean(index, 4, fill = NA, align = "right"))

  # Get the price index - i.e. the rolling annual mean - for the latest year
  latest_index <- full_series_data %>%
    filter(year == if_else(year_end_q == 1, max(series_years) + 1, max(series_years)),
           qtr == year_end_q) %>%
    pull(year_index)

  # Get the indices for all years in the publication
  adjust_series_data <- full_series_data %>%
    mutate(
      # Use the index from the latest year as the base
      year_index_rebase = year_index / latest_index * 100,
      # If financial years were chosen, fix year column
      year = case_when(year_end_q == 1 ~ year - 1, TRUE ~ year),
      year_end = yearqtr) %>%
    # Filter for only the years we are interested in
    filter(year >= min(series_years), year <= max(series_years),
           qtr == year_end_q) %>%
    select(year, year_end, index = year_index_rebase)

  return(adjust_series_data)

}
