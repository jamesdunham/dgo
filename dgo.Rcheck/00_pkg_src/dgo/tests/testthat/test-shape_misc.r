source("setup.r")
suppressMessages({

  context("shape calls")

  test_that("minimal calls are successful", {
    expect_silent(suppressMessages(min_item_call()))
    expect_silent(suppressMessages(min_modifier_call()))
    expect_silent(suppressMessages(min_groupless_call()))
  })

  d_min <- min_item_call()
  d_mod <- min_modifier_call()
  d_nogroups <- min_groupless_call()

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
  })

})
