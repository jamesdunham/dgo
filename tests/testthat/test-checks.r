context("Get periods in which items appear")
suppressMessages({
  rm(list = ls())
  d <- dplyr::data_frame(q1 = c(1, NA), q2 = c(NA, 1),
    t = as.factor(c(1, 2)))
  a <- list(level1 = d, items = c("q1", "q2"), time_id = "t")

  expect_equal(dim(get_question_periods(d, a)), c(2, 3))
  expect_is(get_question_periods(d, a)$t, "factor")
  expect_is(get_question_periods(d, a)$q1, "logical")
  expect_is(get_question_periods(d, a)$q2, "logical")
  expect_equal(get_question_periods(d, a)$q1, c(TRUE, FALSE))
  expect_equal(get_question_periods(d, a)$q2, c(FALSE, TRUE))

  context("Get items appearing in less than min_periods periods")
  rm(list = ls())
  d <- dplyr::data_frame(q1 = c(1, NA), q2 = c(NA, 1),
    t = as.factor(c(1, 2)))
  a <- list(level1 = d, items = c("q1", "q2"), time_id = "t")
  checks <- list(q_when_asked = get_question_periods(d, a))

  expect_equal(get_rare_items_over_t(checks, list(time_id = "t",
        min_periods = 1L)), character(0))
  expect_equal(get_rare_items_over_t(checks, list(time_id = "t",
        min_periods = 2L)), c("q1", "q2"))

  context("Get observed time periods")
  rm(list = ls())
  d <- dplyr::data_frame(time_id = factor(c("1991", "1992")))

  expect_is(get_t(d$time_id), "character")
  expect_equal(get_t(d$time_id), c("1991", "1992"))

  context("Get missing items")
  rm(list = ls())
  d <- dplyr::data_frame(q1 = c(NA, 1), q2 = c(NA, NA))

  expect_equal(get_missing_items(d, c("q1", "q2")), "q2")

  context("Get missing respondents")
  rm(list = ls())
  d <- dplyr::data_frame(q1 = c(NA, 1), q2 = c(NA, NA))

  expect_equal(unname(get_missing_respondents(d)), c(TRUE, FALSE))
})
