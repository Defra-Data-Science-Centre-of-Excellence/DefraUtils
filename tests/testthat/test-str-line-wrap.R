test_that("lines argument gives correct number of new line tags in the correct places", {
  expect_equal(str_line_wrap("a b c d e", 1), "a b c d e")

  expect_equal(str_line_wrap("a b c d", 2), "a b\nc d")
  expect_equal(str_line_wrap("a b c d e", 2), "a b\nc d e")

  expect_equal(str_line_wrap("a b c d", 3), "a\nb c\nd")
  expect_equal(str_line_wrap("a b c d e", 3), "a\nb c\nd e")

  expect_equal(str_line_wrap("a b c d", 4), "a\nb\nc\nd")
  expect_equal(str_line_wrap("a b c d e", 4), "a\nb\nc d\ne")
})

test_that("too many lines give a warning and returns correct result", {
  expect_warning(str_line_wrap("a b c d", 5),
                 paste("There are not enough spaces in the string to give the chosen",
                       "number of line breaks; putting a line break in at every space"))
  expect_equal(suppressWarnings(str_line_wrap("a b c d", 5)), "a\nb\nc\nd")
  expect_no_warning(str_line_wrap("a b c d e", 5))
})
