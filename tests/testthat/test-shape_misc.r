source("setup.r")
suppressMessages({

  d_min <- min_item_call()
  d_mod <- min_modifier_call()

  context("dgirtIn classes")

  test_that("creation works", { 
    expect_is(d_min, "dgirtIn")
    expect_is(d_min, "R6")
    expect_is(d_min$control, "Control")
    expect_true(isS4(d_min$control))
  })

  test_that("*_name arguments appear in control@*_name", {
    expect_identical(d_min$control@item_names, "Q_cces2006_abortion")
    expect_identical(d_min$control@geo_name, "state")
    expect_identical(d_min$control@time_name, "year")
    expect_identical(d_min$control@group_names, "female")
    expect_identical(d_min$control@survey_name, "source")
    expect_identical(d_min$control@weight_name, "weight")
  })

})
