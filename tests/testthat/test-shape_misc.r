source("setup.r")

context("shape calls")

test_that("minimal calls are successful", {
  expect_silent(suppressMessages(shape(item_data = opinion, item_names =
        "abortion", time_name = "year", geo_name = "state", group_names =
        "female")))
  expect_silent(suppressMessages(shape(item_data = opinion, item_names =
        "abortion", time_name = "year", geo_name = "state", group_names =
        "female", modifier_data = states, modifier_names =
        "prop_evangelicals", t1_modifier_names = "prop_evangelicals")))
  expect_silent(suppressMessages(shape(item_data = opinion, item_names =
        "abortion", time_name = "year", geo_name = "state")))
})

test_that("shape accepts aggregate data without individual data", {
  expect_silent(suppressMessages(shape(
        aggregate_data = aggregates,
        aggregate_item_names = unique(aggregates$item),
        time_name = "year", geo_name = "state", group_names =
        c("female", "race3"))))
})

d_min <- suppressMessages(shape(item_data = opinion, item_names = "abortion", time_name =
  "year", geo_name = "state", group_names = "female", survey_name = "source",
weight_name = "weight"))

d_mod <- suppressMessages(shape(item_data = opinion, item_names = "abortion", time_name =
  "year", geo_name = "state", group_names = "female", modifier_data = states,
  modifier_names = "prop_evangelicals", t1_modifier_names =
    "prop_evangelicals"))

d_nogroups <- suppressMessages(shape(item_data = opinion, item_names = "abortion", time_name =
  "year", geo_name = "state"))

d_agg <- suppressMessages(shape(item_data = opinion, aggregate_data = aggregates, item_names = "abortion", time_name =
  "year", geo_name = "state", group_names = c("female", "race3"), survey_name = "source",
weight_name = "weight"))

d_target <- suppressMessages(shape(item_data = opinion, target_data = targets,
  raking = list(~ state), item_names = "abortion", time_name =
  "year", geo_name = "state", group_names = c("female", "race3"), survey_name = "source",
weight_name = "weight"))

context("shape return values are valid")

test_that("dgirtIn creation works", { 
  expect_is(d_min, "dgirtIn")
  expect_is(d_min, "R6")
  expect_is(d_min$control, "Control")
  expect_true(isS4(d_min$control))
})

test_that("*_name arguments appear in control@*_name", {
  expect_identical(d_min$control@item_names, "abortion")
  expect_identical(d_min$control@geo_name, "state")
  expect_identical(d_min$control@time_name, "year")
  expect_identical(d_min$control@group_names, "female")
  expect_identical(d_min$control@survey_name, "source")
  expect_identical(d_min$control@weight_name, "weight")
  expect_identical(d_mod$control@modifier_names, "prop_evangelicals")
  expect_identical(d_nogroups$control@group_names, NULL)
})

test_that("has_* flags are set", {
  expect_true(d_min$control@has_individual_data)
  expect_false(d_min$control@has_modifier_data)
  expect_false(d_min$control@has_target_data)
  expect_false(d_min$control@has_aggregate_data)

  expect_true(d_mod$control@has_individual_data)
  expect_true(d_mod$control@has_modifier_data)
  expect_false(d_mod$control@has_target_data)
  expect_false(d_mod$control@has_aggregate_data)

  expect_true(d_agg$control@has_individual_data)
  expect_false(d_agg$control@has_modifier_data)
  expect_false(d_agg$control@has_target_data)
  expect_true(d_agg$control@has_aggregate_data)

  expect_true(d_target$control@has_individual_data)
  expect_false(d_target$control@has_modifier_data)
  expect_true(d_target$control@has_target_data)
  expect_false(d_target$control@has_aggregate_data)
})

test_that("setDT_data works as expected", {
  data(opinion)
  expect_is(opinion, "data.frame")
  expect_true(!inherits(opinion, "data.table"))
  item_data <- dgo:::set_copy_dt(opinion)
  expect_is(item_data, "data.table")
  item_data <- NULL
  expect_true(!is.null(opinion))
  item_data <- dgo:::set_copy_dt(item_data)
  expect_true(is.null(item_data))
})

test_that("min_t_filter restricts individual data", {
  expect_error(suppressMessages(shape(minimal_individual_data, item_names = 'item_1',
    time_name = 'period', geo_name = 'geo', group_names = 'pid',
    min_t_filter = 3), 'no items remaining'))
})

test_that("min_survey_filter restricts individual data", {
  minimal_individual_data[, survey := 'survey1']
  expect_error(suppressMessages(shape(minimal_individual_data, item_names = 'item_1',
    time_name = 'period', geo_name = 'geo', group_names = 'pid',
    survey_name = 'survey', min_survey_filter = 2)), 'no items remaining')
})

test_that("aggregated data can be used with geographic data, without demographic groups or individual data", {
  data(aggregates)
  data(states)
  data.table::setDT(aggregates)
  aggregates = aggregates[, .(n_grp = sum(n_grp), s_grp = sum(s_grp)), by = c('year', 'state', 'item')]
  expect_silent(suppressMessages(shape(aggregate_data = aggregates, geo_name = 'state',
      time_name = 'year', modifier_data = states, modifier_names =
        'prop_hispanic')))
})
