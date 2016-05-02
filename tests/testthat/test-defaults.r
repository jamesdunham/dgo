source("setup.r")
suppressMessages({

  d_min <- min_item_call()

  context("defaults")

  test_that('filters take correct defaults', {
    expect_identical(d_min$control@time_filter, sort(unique(opinion$year)))
    expect_identical(d_min$control@geo_filter, sort(unique(opinion$state)))
    expect_identical(d_min$control@min_survey_filter, 1L)
    expect_identical(d_min$control@min_t_filter, 1L)
  })

  test_that('constant_item defaults to TRUE', {
    expect_true(min_item_call()$control@constant_item)
  })

})
