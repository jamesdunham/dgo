context("dichotomizing item responses")

test_that("dichotomizing item responses works", {
  expect_equal(dichotomize_r(0:1), data.table::data.table(X_gt1 = 0:1))
  expect_equal(dichotomize_r(NA), data.table::data.table(X_gt1 = as.numeric(NA)))
  expect_equal(dichotomize_r(c(1:2, NA)), data.table::data.table(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_r(c(0:1, NA)), data.table::data.table(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_r(c(0, 2, NA)), data.table::data.table(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_r(1:3), data.table::data.table(X_gt1 = c(0, 1, 1),
                                           X_gt2 = c(0, 0, 1)))
  expect_equal(dichotomize_r(as.factor(0:1)), data.table::data.table(X_gt1 = 0:1))
  expect_equal(dichotomize_r(as.factor(NA)), data.table::data.table(X_gt1 = as.numeric(NA)))
  expect_equal(dichotomize_r(as.factor(c(1:2, NA))), data.table::data.table(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_r(as.factor(c(0:1, NA))),
               data.table::data.table(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_r(as.factor(c(0, 2, NA))),
               data.table::data.table(X_gt1 = c(0, 1, NA)))
  expect_equal(dichotomize_r(as.factor(1:3)), data.table::data.table(X_gt1 = c(0, 1, 1),
                                           X_gt2 = c(0, 0, 1)))

  expect_equal(dichotomize_r(as.ordered(c("a", "b"))), data.table::data.table(X_gt1 = 0:1))
  expect_equal(dichotomize_r(ordered(c("a", "b"),levels = c("b", "a"))),
               data.table::data.table(X_gt1 = c(1, 0)))

  expect_warning(dichotomize_r("a"), "NAs introduced by coercion", ignore.case = TRUE)
})
