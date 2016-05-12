# library(testthat)
# test_levels <- c(1:4, NA)
# test_values <- sample(test_levels, 100, TRUE) 
# Rcpp::sourceCpp('~/projects/dgirt/src/dichotomize_cpp.cpp')

# Rcpp::sourceCpp('~/projects/dgirt/src/dichotomize.cpp')
# dichotomize_cpp(c(c(1, 3), NA))
# dichotomize_cpp(1:3)

test_that("dichotomizing item responses works", {
  expect_equal(dichotomize_cpp(0:1), data.frame(X_gt1 = 0:1))
  expect_equal(dichotomize_cpp(NA), data.frame(X_gt1 = as.numeric(NA)))
  expect_equal(dichotomize_cpp(c(1:2, NA)), data.frame(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_cpp(c(0:1, NA)), data.frame(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_cpp(c(0, 2, NA)), data.frame(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_cpp(1:3), data.frame(X_gt1 = c(0, 1, 1),
                                           X_gt2 = c(0, 0, 1)))
  expect_equal(dichotomize_cpp(as.factor(0:1)), data.frame(X_gt1 = 0:1))
  expect_equal(dichotomize_cpp(as.factor(NA)), data.frame(X_gt1 = as.numeric(NA)))
  expect_equal(dichotomize_cpp(as.factor(c(1:2, NA))), data.frame(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_cpp(as.factor(c(0:1, NA))),
               data.frame(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_cpp(as.factor(c(0, 2, NA))),
               data.frame(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_cpp(as.factor(1:3)), data.frame(X_gt1 = c(0, 1, 1),
                                           X_gt2 = c(0, 0, 1)))

  expect_equal(dichotomize_cpp(as.ordered(c("a", "b"))), data.frame(X_gt1 = 0:1))
  expect_equal(dichotomize_cpp(ordered(c("a", "b"),levels = c("b", "a"))),
               data.frame(X_gt1 = c(1, 0)))

  expect_error(dichotomize_cpp("a"), "not compatible")
})
# x = min_item_call()
# x$n_vec
