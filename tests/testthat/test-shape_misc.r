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

d_min <- shape(item_data = opinion, item_names = "abortion", time_name =
  "year", geo_name = "state", group_names = "female", survey_name = "source",
weight_name = "weight")
d_mod <- shape(item_data = opinion, item_names = "abortion", time_name =
  "year", geo_name = "state", group_names = "female", modifier_data = states,
  modifier_names = "prop_evangelicals", t1_modifier_names = "prop_evangelicals")
d_nogroups <- shape(item_data = opinion, item_names = "abortion", time_name =
  "year", geo_name = "state")

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

test_that("setDT_data works as expected", {
  data(opinion)
  expect_is(opinion, "data.frame")
  expect_true(!inherits(opinion, "data.table"))
  item_data <- set_copy_dt(opinion)
  expect_is(item_data, "data.table")
  item_data <- NULL
  expect_true(!is.null(opinion))
  item_data <- set_copy_dt(item_data)
  expect_true(is.null(item_data))
})

