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

  # TODO: debug
  data(aggregates)
  d_agg = min_agg_call()

  disjoint_geo_time <- data.table::data.table(
    race = "white", female = "male", year = 0, item = "Q_cces2006_abortion",
    state = "foo", n_grp = 10, s_grp = 10)
  aggregates <- data.table::rbindlist(list(aggregates, disjoint_geo_time))
  d_disjoint_agg <- min_agg_call(aggregate_data = aggregates)

  test_that("time and geo defaults include values in aggregate_data", {
    expect_identical(sort(unique(c(opinion$year, aggregates$year))),
                     d_agg$control@time_filter)
    expect_identical(sort(unique(c(opinion$state, aggregates$state))),
                     d_agg$control@geo_filter)
    expect_identical(sort(unique(c(opinion$year, 0))),
                     d_disjoint_agg$control@time_filter)
    expect_identical(sort(unique(c(opinion$state, "foo"))),
                     d_disjoint_agg$control@geo_filter)
  })

  test_that('constant_item defaults to TRUE', {
    expect_true(min_item_call()$control@constant_item)
  })

})
