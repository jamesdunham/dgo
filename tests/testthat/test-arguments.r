context("Argument defaults")

context("Wrap dplyr::as.tbl")

expect_is(as_tbl(NULL), "NULL")
expect_is(as_tbl(data.frame()), "tbl")

context("Factorize variables")
rm(list = ls())
d <- dplyr::data_frame(q1 = 1, t = 1, geo = 1, female = 1, poll = 1,
  weight = 1)
a <- list(level1 = d, items = "q1", time_id = "t", geo_id = "geo",
  groups = "female", survey_id = "poll", survey_weight = "weight")

expect_silent(factorize_arg_vars(NULL, a))
expect_is(factorize_arg_vars(d, a)$t, "factor")
expect_is(factorize_arg_vars(d, a)$female, "factor")
expect_is(factorize_arg_vars(d, a)$geo, "factor")
expect_is(factorize_arg_vars(d, a)$poll, "factor")

# If arguments weren't passed they should take their default
expect_equal(set_arg_defaults(list())$separate_periods, FALSE)
expect_equal(set_arg_defaults(list())$difficulty_count, 1L)
expect_equal(set_arg_defaults(list())$min_periods, 1L)
expect_equal(set_arg_defaults(list())$min_surveys, 1L)
expect_equal(set_arg_defaults(list())$constant_item, TRUE)
expect_equal(set_arg_defaults(list())$silent, FALSE)

# If arguments were passed they should be unchanged
expect_equal(set_arg_defaults(list(separate_periods = TRUE))$separate_periods,
  TRUE)
expect_equal(set_arg_defaults(list(difficulty_count = 2))$difficulty_count,
  2L)
expect_equal(set_arg_defaults(list(min_periods = 2))$min_periods, 2L)
expect_equal(set_arg_defaults(list(min_surveys = 2))$min_surveys, 2L)
expect_equal(set_arg_defaults(list(constant_item = FALSE))$constant_item,
  FALSE)
expect_equal(set_arg_defaults(list(silent = TRUE))$silent, TRUE)

context("Argument lengths")
expect_error(check_arg_lengths(time_id = c("var1", "var2")))
expect_error(check_arg_lengths(time_id = character(0)))
expect_error(check_arg_lengths(geo_id = c("var1", "var2")))
expect_error(check_arg_lengths(survey_weight = c("var1", "var2")))
expect_error(check_arg_lengths(survey_id = c("var1", "var2")))
expect_error(check_arg_lengths(group_proportion = c("var1", "var2")))

context("Argument names")
d <- dplyr::data_frame(q1 = 1, t = 1, geo = 1, female = 1, poll = 1,
  weight = 1)
d_args <- list(level1 = d, items = "q1", time_id = "t", geo_id = "geo",
  groups = "female", survey_id = "poll", survey_weight = "weight")

expect_silent(check_arg_names(d_args))
expect_error(check_arg_names(list(level1 = d, items = "q")),
  "element\\s* of \\.level1")
expect_error(check_arg_names(list(level1 = d, items = "T")),
  "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, items = 1)),
  "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, items = NA)),
  "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, time_id = "T")),
  "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, groups = "Female")),
  "element\\s* of .level1")
expect_error(check_arg_names(list(level1 = d, geo_id = "Geo")),
  "element\\s* of .level1")

context("Argument types")
rm(list = ls())
d <- dplyr::data_frame(q1 = 1, t = 1, geo = 1, female = 1, poll = 1,
  weight = 1)
a <- set_arg_defaults(list(level1 = d, items = "q1", time_id = "t",
    geo_id = "geo", groups = "female", survey_id = "poll",
    survey_weight = "weight"))

expect_silent(check_arg_types(a))
expect_silent(check_arg_types(c(a[-1], list(level1 =
  dplyr::as.tbl(data.frame())))))
expect_error(check_arg_types(c(a[-1], list(level1 = NA))),
  "inherit from data.frame")
expect_error(check_arg_types(c(a[-1])), "is NULL")
expect_error(check_arg_types(c(a[-2], list(items = 1))),
  "should be a character vector")
expect_error(check_arg_types(c(a[-3], list(time_id = NA))),
  "should be a character vector")
expect_error(check_arg_types(c(a[-4], list(geo_id = list()))),
  "should be a character vector")
expect_error(check_arg_types(c(a[-5], list(groups = data.frame()))),
  "should be a character vector")
expect_error(check_arg_types(c(a[-6], list(survey_id = NULL))),
  "should be a character vector")
expect_error(check_arg_types(c(a[-7], list(survey_weight = 1))),
  "should be a character vector")
expect_error(check_arg_types(c(a[-8], list(separate_periods = "TRUE"))),
  "should be TRUE or FALSE")
expect_error(check_arg_types(c(a[-9], list(difficulty_count = 0))),
  "should be a positive integer")
expect_error(check_arg_types(c(a[-9], list(difficulty_count = ""))),
  "should be a positive integer")
expect_error(check_arg_types(c(a[-10], list(min_surveys = 0))),
  "should be a positive integer")
expect_error(check_arg_types(c(a[-11], list(min_periods = 0))),
  "should be a positive integer")
expect_error(check_arg_types(c(a[-13], list(silent = 1))),
  "should be TRUE or FALSE")
