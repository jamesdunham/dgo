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

  test_that("time and geo defaults include values in aggregate_data", {
    d_agg = min_agg_call()
    expect_equal(sort(unique(c(opinion$year, aggregates$year))),
                     d_agg$control@time_filter)
    expect_equal(sort(unique(c(opinion$state, aggregates$state))),
                     d_agg$control@geo_filter)
  })

  disjoint_geo_time <- data.table::data.table(
    race = "white", female = "male", year = 0, item = "Q_cces2006_abortion",
    state = "foo", n_grp = 10, s_grp = 10)
  disjoint_geo_time <- data.table::rbindlist(list(dgirt:::aggregates, disjoint_geo_time))
  d_disjoint_agg <- min_agg_call(aggregate_data = disjoint_geo_time)

  test_that("time and geo defaults include disjoint values in aggregate_data", {
    expect_equal(sort(unique(c(opinion$year, 0))), d_disjoint_agg$control@time_filter)
    expect_equal(sort(unique(c(opinion$state, "foo"))), d_disjoint_agg$control@geo_filter)
  })

  test_that('constant_item defaults to TRUE', {
    expect_true(min_item_call()$control@constant_item)
  })

})
