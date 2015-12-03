context("Wrap dplyr::as.tbl")

expect_is(as_tbl(NULL), "NULL")
expect_is(as_tbl(data.frame()), "tbl")

context("Argument defaults")

# If arguments weren't passed they should take their default
expect_equal(set_arg_defaults(list())$separate_periods, FALSE)
expect_equal(set_arg_defaults(list())$difficulty_count, 1L)
expect_equal(set_arg_defaults(list())$min_periods, 1L)
expect_equal(set_arg_defaults(list())$min_surveys, 1L)
expect_equal(set_arg_defaults(list())$constant_item, TRUE)
expect_equal(set_arg_defaults(list())$silent, FALSE)

# If arguments were passed they should be unchanged
expect_equal(set_arg_defaults(list(separate_periods = TRUE))$separate_periods, TRUE)
expect_equal(set_arg_defaults(list(difficulty_count = 2))$difficulty_count, 2L)
expect_equal(set_arg_defaults(list(min_periods = 2))$min_periods, 2L)
expect_equal(set_arg_defaults(list(min_surveys = 2))$min_surveys, 2L)
expect_equal(set_arg_defaults(list(constant_item = FALSE))$constant_item, FALSE)
expect_equal(set_arg_defaults(list(silent = TRUE))$silent, TRUE)

context("Argument lengths")
expect_error(check_arg_lengths(time_id = c("var1", "var2")))
expect_error(check_arg_lengths(time_id = character(0)))
expect_error(check_arg_lengths(geo_id = c("var1", "var2")))
expect_error(check_arg_lengths(survey_weight = c("var1", "var2")))
expect_error(check_arg_lengths(survey_id = c("var1", "var2")))
expect_error(check_arg_lengths(group_proportion = c("var1", "var2")))

context("Argument names")
d <- dplyr::data_frame(q1 = 1, t = 1, geo = 1, female = 1, poll = 1, weight = 1)
d_args <- list(level1 = d, items = "q1", time_id = "t", geo_id = "geo", groups = "female", 
  survey_id = "poll", survey_weight = "weight")

expect_silent(check_arg_names(d_args))
expect_error(check_arg_names(list(level1 = d, items = "q")), "element\\s* of \\.level1")
expect_error(check_arg_names(list(level1 = d, items = "T")), "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, items = 1)), "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, items = NA)), "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, time_id = "T")), "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, groups = "Female")), " element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, geo_id = "Geo")), "element\\s* of .level1")

# TODO: add required elements to level1 expect_error(check_arg_names(list(level1
# = d, level2_modifiers = 'Geo')), 'but .level2 is NULL')
# expect_error(check_arg_names(list(level1 = d, c(d_args,
# level2_period1_modifiers = 'Geo')), 'given but .level2 is NULL')

context("Argument types")
rm(list = ls())
d <- dplyr::data_frame(q1 = 1, t = 1, geo = 1, female = 1, poll = 1, weight = 1)
a <- set_arg_defaults(list(level1 = d, items = "q1", time_id = "t", geo_id = "geo", 
  groups = "female", survey_id = "poll", survey_weight = "weight"))

expect_silent(check_arg_types(a))
expect_silent(check_arg_types(c(a[-1], list(level1 = dplyr::as.tbl(data.frame())))))
expect_error(check_arg_types(c(a[-1], list(level1 = NA))), "inherit from data.frame")
expect_error(check_arg_types(c(a[-1])), "is NULL")
expect_error(check_arg_types(c(a[-2], list(items = 1))), "should be a character vector")
expect_error(check_arg_types(c(a[-3], list(time_id = NA))), "should be a character vector")
expect_error(check_arg_types(c(a[-4], list(geo_id = list()))), "should be a character vector")
expect_error(check_arg_types(c(a[-5], list(groups = data.frame()))), "should be a character vector")
expect_error(check_arg_types(c(a[-6], list(survey_id = NULL))), "should be a character vector")
expect_error(check_arg_types(c(a[-7], list(survey_weight = 1))), "should be a character vector")
expect_error(check_arg_types(c(a[-8], list(separate_periods = "TRUE"))), "should be TRUE or FALSE")
expect_error(check_arg_types(c(a[-9], list(difficulty_count = 0))), "should be a positive integer")
expect_error(check_arg_types(c(a[-9], list(difficulty_count = ""))), "should be a positive integer")
expect_error(check_arg_types(c(a[-10], list(min_surveys = 0))), "should be a positive integer")
expect_error(check_arg_types(c(a[-11], list(min_periods = 0))), "should be a positive integer")
expect_error(check_arg_types(c(a[-13], list(silent = 1))), "should be TRUE or FALSE")

context("Factorize variables")
rm(list = ls())
d <- dplyr::data_frame(q1 = 1, t = 1, geo = 1, female = 1, poll = 1, weight = 1)
a <- list(level1 = d, items = "q1", time_id = "t", geo_id = "geo", groups = "female", 
  survey_id = "poll", survey_weight = "weight")

expect_silent(factorize_arg_vars(NULL, a))
expect_is(factorize_arg_vars(d, a)$t, "factor")
expect_is(factorize_arg_vars(d, a)$female, "factor")
expect_is(factorize_arg_vars(d, a)$geo, "factor")
expect_is(factorize_arg_vars(d, a)$poll, "factor")

context("Get periods in which items appear")
rm(list = ls())
d <- dplyr::data_frame(q1 = c(1, NA), q2 = c(NA, 1), t = as.factor(c(1, 2)))
a <- list(level1 = d, items = c("q1", "q2"), time_id = "t")

expect_equal(dim(get_question_periods(d, a)), c(2, 3))
expect_is(get_question_periods(d, a)$t, "factor")
expect_is(get_question_periods(d, a)$q1, "logical")
expect_is(get_question_periods(d, a)$q2, "logical")
expect_equal(get_question_periods(d, a)$q1, c(TRUE, FALSE))
expect_equal(get_question_periods(d, a)$q2, c(FALSE, TRUE))

context("Get items appearing in less than min_periods periods")
rm(list = ls())
d <- dplyr::data_frame(q1 = c(1, NA), q2 = c(NA, 1), t = as.factor(c(1, 2)))
a <- list(level1 = d, items = c("q1", "q2"), time_id = "t")
checks <- list(q_when_asked = get_question_periods(d, a))

expect_equal(get_rare_items_over_t(checks, list(time_id = "t", min_periods = 1L)), 
  character(0))
expect_equal(get_rare_items_over_t(checks, list(time_id = "t", min_periods = 2L)), 
  c("q1", "q2"))

context("Get items appearing in less than min_surveys polls")
# d_poll_args = poll_check = list(q.which.asked = get_question_polls(d_t,
# d_t_args))

context("Get observed time periods")
rm(list = ls())
d <- dplyr::data_frame(time_id = factor(c("1991", "1992")))

expect_is(get_t(d$time_id), "character")
expect_equal(get_t(d$time_id), c("1991", "1992"))

context("Get missing items")
rm(list = ls())
d <- dplyr::data_frame(q1 = c(NA, 1), q2 = c(NA, NA))

expect_equal(get_missing_items(d), "q2")

context("Get missing respondents")
rm(list = ls())
d <- dplyr::data_frame(q1 = c(NA, 1), q2 = c(NA, NA))

expect_equal(unname(get_missing_respondents(d)), c(TRUE, FALSE))

context("Subset to estimation periods")
rm(list = ls())

d <- data.frame(dgirt_t = as.factor(c(1991, 1991, 1992)))
a <- list(use_t = c(1991))
subset_to_estimation_periods(d, a) 
