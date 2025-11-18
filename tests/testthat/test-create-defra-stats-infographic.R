test_that("function runs without failing", {

  title <- "Energy use on farms in England"
  subtitle <- "2023/24"
  source <- "Source: Defra, Farm Business Survey England 2023/24"
  icons <- c("tractor", "sun", "barn", "form")
  key_points <- list(
    "Red diesel was used<br>by **98%** of farms",
    "**32% generate renewable energy**<br>(mostly solar power)",
    "**84%** of solar-generating farm<br>businesses have panels installed on<br>farm building rooftops",
    "**20%** of farms had<br>conducted a **carbon audit**")

  expect_no_failure(create_defra_stats_infographic(title, subtitle, source, key_points, icons))

})
