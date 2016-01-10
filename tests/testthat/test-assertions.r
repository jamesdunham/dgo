context("assertions")
suppressMessages({
  testthat::expect_error(assertthat::assert_that(none_empty(list())),
    "has an empty dimension")
  testthat::expect_silent(assertthat::assert_that(none_empty(list(1))))

  ab <- data.frame(a = 1, b = 2)
  els <- c("b", "c")
  expect_silent(assertthat::assert_that(has_all_names(ab, c("a"))))
  expect_silent(assertthat::assert_that(has_all_names(ab, c("a", "b"))))
  expect_error(assertthat::assert_that(has_all_names(ab, "c")),
    "does not have name")
  expect_error(assertthat::assert_that(has_all_names(ab, els)),
    "does not have name")
})
