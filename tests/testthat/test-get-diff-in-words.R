# Percentage changes ####
test_that("normal percentage changes are returned correctly", {
  # Option 1
  expect_equal(get_diff_in_words(1, .1, "10%", curr_rnd = "100"),
               "increased by 10% to 100")
  expect_equal(get_diff_in_words(1, -.1, "10%", curr_rnd = "100"),
               "decreased by 10% to 100")

  # Option 2
  expect_equal(get_diff_in_words(2, .1, "10%", curr_rnd = "100"),
               "increasing by 10% to 100")
  expect_equal(get_diff_in_words(2, -.1, "10%", curr_rnd = "100"),
               "decreasing by 10% to 100")

  # Option 3
  expect_equal(get_diff_in_words(3, .1, "10%", curr_rnd = "100"),
               "100, an increase of 10%")
  expect_equal(get_diff_in_words(3, -.1, "10%", curr_rnd = "100"),
               "100, a decrease of 10%")

  # Option 4
  expect_equal(get_diff_in_words(4, .1, "10%"), "10% higher")
  expect_equal(get_diff_in_words(4, -.1, "10%"), "10% lower")

  # Option 5
  expect_equal(get_diff_in_words(5, .1, "10%"), "a rise of 10%")
  expect_equal(get_diff_in_words(5, -.1, "10%"), "a fall of 10%")

  # Option 6
  expect_equal(get_diff_in_words(6, .1, "10%"), "rose by 10%")
  expect_equal(get_diff_in_words(6, -.1, "10%"), "fell by 10%")
})

test_that("marginal changes are returned correctly", {
  # Option 1
  expect_equal(get_diff_in_words(1, .01, "0%", curr_rnd = "100"),
               "marginally increased to 100")
  expect_equal(get_diff_in_words(1, -.01, "0%", curr_rnd = "100"),
               "marginally decreased to 100")

  # Option 2
  expect_equal(get_diff_in_words(2, .01, "0%", curr_rnd = "100"),
               "marginally increasing to 100")
  expect_equal(get_diff_in_words(2, -.01, "0%", curr_rnd = "100"),
               "marginally decreasing to 100")

  # Option 3
  expect_equal(get_diff_in_words(3, .01, "0%", curr_rnd = "100"),
               "100, a negligible change")
  expect_equal(get_diff_in_words(3, -.01, "0%", curr_rnd = "100"),
               "100, a negligible change")

  # Option 4
  expect_equal(get_diff_in_words(4, .01, "0%"), "marginally higher")
  expect_equal(get_diff_in_words(4, -.01, "0%"), "marginally lower")

  # Option 5
  expect_equal(get_diff_in_words(5, .01, "0%"), "a negligible change")
  expect_equal(get_diff_in_words(5, -.01, "0%"), "a negligible change")

  # Option 6
  expect_equal(get_diff_in_words(6, .01, "0%"), "marginally rose")
  expect_equal(get_diff_in_words(6, -.01, "0%"), "marginally fell")
})

test_that("no change is return correctly", {
  # Option 1
  expect_equal(get_diff_in_words(1, 0, "0%", curr_rnd = "100"),
               "did not change from 100")

  # Option 2
  expect_equal(get_diff_in_words(2, 0, "0%", curr_rnd = "100"),
               "not changing from 100")

  # Option 3
  expect_equal(get_diff_in_words(3, 0, "0%", curr_rnd = "100"),
               "100, no change")

  # Option 4
  expect_equal(get_diff_in_words(4, 0, "0%"), "the same as")

  # Option 5
  expect_equal(get_diff_in_words(5, 0, "0%"), "no change")

  # Option 6
  expect_equal(get_diff_in_words(6, 0, "0%"), "did not change")
})

test_that("doubling etc. is returned corrently", {
  # Option 1
  # Large increases
  expect_equal(get_diff_in_words(1, 14.5, "1,450%", curr_rnd = "100"),
               "increased more than 15-fold to 100")
  expect_equal(get_diff_in_words(1, 11, "1,100%", curr_rnd = "100"),
               "increased 12-fold to 100")
  expect_equal(get_diff_in_words(1, 7.7, "770%", curr_rnd = "100"),
               "increased more than eight-fold to 100")
  expect_equal(get_diff_in_words(1, 4, "400%", curr_rnd = "100"),
               "increased five-fold to 100")
  # Quadrupling
  expect_equal(get_diff_in_words(1, 3.2, "320%", curr_rnd = "100"),
               "more than quadrupled to 100")
  expect_equal(get_diff_in_words(1, 3, "300%", curr_rnd = "100"),
               "quadrupled to 100")
  # Tripling
  expect_equal(get_diff_in_words(1, 2.4, "240%", curr_rnd = "100"),
               "more than tripled to 100")
  expect_equal(get_diff_in_words(1, 2, "200%", curr_rnd = "100"),
               "tripled to 100")
  # Doubling
  expect_equal(get_diff_in_words(1, 1.1, "110%", curr_rnd = "100"),
               "more than doubled to 100")
  expect_equal(get_diff_in_words(1, 1, "100%", curr_rnd = "100"),
               "doubled to 100")
  # Halving
  expect_equal(get_diff_in_words(1, -.55, "55%", curr_rnd = "100"),
               "fell by around half to 100")
  expect_equal(get_diff_in_words(1, -.51, "51%", curr_rnd = "100"),
               "halved to 100")
  expect_equal(get_diff_in_words(1, -.5, "50%", curr_rnd = "100"),
               "halved to 100")
  # Falling by 3/4
  expect_equal(get_diff_in_words(1, -.78, "78%", curr_rnd = "100"),
               "fell by around three quarters to 100")
  expect_equal(get_diff_in_words(1, -.76, "76%", curr_rnd = "100"),
               "fell by three quarters to 100")
  expect_equal(get_diff_in_words(1, -.75, "75%", curr_rnd = "100"),
               "fell by three quarters to 100")

  # Option 2
  expect_equal(get_diff_in_words(2, 3, "300%", curr_rnd = "100"),
               "quadrupling to 100")
  expect_equal(get_diff_in_words(2, 2, "200%", curr_rnd = "100"),
               "tripling to 100")
  expect_equal(get_diff_in_words(2, 1, "100%", curr_rnd = "100"),
               "doubling to 100")
  expect_equal(get_diff_in_words(2, -.5, "50%", curr_rnd = "100"),
               "halving to 100")

  # Option 3
  expect_equal(get_diff_in_words(3, 4, "400%", curr_rnd = "100"),
               "100, a considerable increase")
  expect_equal(get_diff_in_words(3, 3, "300%", curr_rnd = "100"),
               "100, a considerable increase")
  expect_equal(get_diff_in_words(3, 2, "200%", curr_rnd = "100"),
               "100, a considerable increase")
  expect_equal(get_diff_in_words(3, 1, "100%", curr_rnd = "100"),
               "100, a considerable increase")
  expect_equal(get_diff_in_words(3, -.5, "50%", curr_rnd = "100"),
               "100, a decrease of 50%")

  # Option 4
  expect_equal(get_diff_in_words(4, 4, "400%"), "considerably higher")
  expect_equal(get_diff_in_words(4, 3, "300%"), "quadruple")
  expect_equal(get_diff_in_words(4, 2, "200%"), "triple")
  expect_equal(get_diff_in_words(4, 1, "100%"), "double")
  expect_equal(get_diff_in_words(4, -.5, "50%"), "half")

  # Option 5
  expect_equal(get_diff_in_words(5, 4, "300%"), "a considerable rise")
  expect_equal(get_diff_in_words(5, 3, "300%"), "a considerable rise")
  expect_equal(get_diff_in_words(5, 2, "200%"), "a considerable rise")
  expect_equal(get_diff_in_words(5, 1, "100%"), "a considerable rise")
  expect_equal(get_diff_in_words(5, -.5, "50%"), "a fall of 50%")

  # Option 6
  expect_equal(get_diff_in_words(6, 4, "400%"), "considerably rose")
  expect_equal(get_diff_in_words(6, 3, "300%"), "quadrupled")
  expect_equal(get_diff_in_words(6, 2, "200%"), "tripled")
  expect_equal(get_diff_in_words(6, 1, "100%"), "doubled")
  expect_equal(get_diff_in_words(6, -.5, "50%"), "halved")
})

# Percentage point changes ####
test_that("percentage point changes are returned correctly", {
  # Option 1
  expect_equal(get_diff_in_words(1, .1, "10 points", curr_rnd = "100", points = T),
               "increased by 10 points to 100")
  expect_equal(get_diff_in_words(1, -.1, "10 points", curr_rnd = "100"),
               "decreased by 10 points to 100")

  # Option 2
  expect_equal(get_diff_in_words(2, .1, "10 points", curr_rnd = "100"),
               "increasing by 10 points to 100")
  expect_equal(get_diff_in_words(2, -.1, "10 points", curr_rnd = "100"),
               "decreasing by 10 points to 100")

  # Option 3
  expect_equal(get_diff_in_words(3, .1, "10 points", curr_rnd = "100"),
               "100, an increase of 10 points")
  expect_equal(get_diff_in_words(3, -.1, "10 points", curr_rnd = "100"),
               "100, a decrease of 10 points")

  # Option 4
  expect_equal(get_diff_in_words(4, .1, "10 points"), "10 points higher")
  expect_equal(get_diff_in_words(4, -.1, "10 points"), "10 points lower")

  # Option 5
  expect_equal(get_diff_in_words(5, .1, "10 points"), "a rise of 10 points")
  expect_equal(get_diff_in_words(5, -.1, "10 points"), "a fall of 10 points")

  # Option 6
  expect_equal(get_diff_in_words(6, .1, "10 points"), "rose by 10 points")
  expect_equal(get_diff_in_words(6, -.1, "10 points"), "fell by 10 points")
})
