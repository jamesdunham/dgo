# library(testthat)
# test_levels <- c(1:4, NA)
# test_values <- sample(test_levels, 100, TRUE) 
# Rcpp::sourceCpp('~/projects/dgirt/src/is_greater.cpp')
# is_greater(c(c(1, 3), NA))

test_that("dichotomizing item responses works", {
  expect_equal(is_greater(0:1), data.frame(`_gt1` = 0:1))
  expect_equal(is_greater(NA), data.frame(`_gt1` = as.numeric(NA)))
  expect_equal(is_greater(c(1:2, NA)), data.frame(`_gt1` = c(0, 1, NA)))
  expect_equal(is_greater(c(0:1, NA)), data.frame(`_gt1` = c(0, 1, NA)))
  expect_equal(is_greater(c(0, 2, NA)), data.frame(`_gt1` = c(0, 1, NA)))
  expect_equal(is_greater(1:3), data.frame(`_gt1` = c(0, 1, 1),
                                           `_gt2` = c(0, 0, 1)))
  expect_equal(is_greater(as.factor(0:1)), data.frame(`_gt1` = 0:1))
  expect_equal(is_greater(as.factor(NA)), data.frame(`_gt1` = as.numeric(NA)))
  expect_equal(is_greater(as.factor(c(1:2, NA))), data.frame(`_gt1` = c(0, 1, NA)))
  expect_equal(is_greater(as.factor(c(0:1, NA))),
               data.frame(`_gt1` = c(0, 1, NA)))
  expect_equal(is_greater(as.factor(c(0, 2, NA))),
               data.frame(`_gt1` = c(0, 1, NA)))
  expect_equal(is_greater(as.factor(1:3)), data.frame(`_gt1` = c(0, 1, 1),
                                           `_gt2` = c(0, 0, 1)))

  expect_equal(is_greater(as.ordered(c("a", "b"))), data.frame(`_gt1` = 0:1))
  expect_equal(is_greater(ordered(c("a", "b"),levels = c("b", "a"))),
               data.frame(`_gt1` = c(1, 0)))

  expect_error(is_greater("a"), "not compatible")
})
# x = min_item_call()
# x$n_vec
