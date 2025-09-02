# CPIH INDEX 00: ALL ITEMS 2015=100 --------------------------------------------

cpih_deflator_data <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
destfile <- "data-raw/cpih_deflators.csv"
download.file(cpih_deflator_data, destfile)

cpih_deflators <- readr::read_csv(destfile, show_col_types = FALSE)

# Check that the link still works - today's date should be before the next release date
next_release <- as.Date(dplyr::pull(dplyr::filter(cpih_deflators, Title == "Next release"), 2), "%d %B %Y")

if (Sys.Date() > next_release) {
  stop("CPIH deflator link may be broken, check data-raw/cpih_deflators.R")
}

usethis::use_data(cpih_deflators, overwrite = TRUE)
