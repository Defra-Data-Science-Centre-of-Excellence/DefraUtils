test_that("set_databricks_pat returns input correctly", {
  # mock input
  mock_input <- "abc123"
  
  # make function run with mock_input as the input
  stub(set_databricks_pat, "readline", mock_input)
  
  # test expected return
  expect_equal(set_databricks_pat(), mock_input)
})
