# Gross domestic product at market prices:Implied deflator:SA ------------------

gdp_deflator_data <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ybgb/ukea"
destfile <- "data-raw/gdp_deflators.csv"
download.file(gdp_deflator_data, destfile)

gdp_deflators <- readr::read_csv(destfile, show_col_types = FALSE)

# Check that the link still works - today's date should be before the next release date
next_release <- as.Date(dplyr::pull(dplyr::filter(gdp_deflators, Title == "Next release"), 2), "%d %B %Y")

if (Sys.Date() > next_release) {
  stop("GDP deflator link is broken, check data-raw/gdp-deflators.R")
}

usethis::use_data(gdp_deflators, overwrite = TRUE)
