test_that("oxford comma argument works", {
  expect_equal(create_list(c("apple", "banana", "pear")),
               "apple, banana and pear")
  expect_equal(create_list(c("apple", "banana", "pear"), oxford_comma = T),
               "apple, banana, and pear")
})

test_that("descriptor argument works", {
  expect_equal(create_list(string_vector = c("apple", "banana"),
                           descriptor = c("fruit", "fruits")),
               "apple and banana fruits")
  expect_equal(create_list(string_vector = "apple",
                           descriptor = c("fruit", "fruits")),
               "apple fruit")
})

test_that("last connector argument works", {
  expect_equal(create_list(c("apple", "banana", "pear"), last_connector = "or"),
               "apple, banana or pear")
})

test_that("case argument works", {
  expect_equal(create_list(c("APPLE", "pear"), case = "lower"),
               "apple and pear")
  expect_equal(create_list(c("APPLE", "pear"), case = "upper"),
               "APPLE AND PEAR")
  expect_equal(create_list(c("APPLE", "pear"), case = "sentence"),
               "Apple and pear")
  expect_equal(create_list(c("APPLE", "pear"), case = "title"),
               "Apple And Pear")
})
