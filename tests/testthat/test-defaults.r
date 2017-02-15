source("setup.r")
suppressMessages({

  context("default values")

  test_that('filters take correct defaults', {
    d_min <- shape(item_data = opinion,
                   item_names = "abortion",
                   time_name = "year",
                   geo_name = "state",
                   group_names = "female")
    expect_identical(d_min$control@time_filter, sort(unique(opinion$year)))
    expect_identical(d_min$control@geo_filter, sort(unique(opinion$state)))
    expect_identical(d_min$control@min_survey_filter, 1L)
    expect_identical(d_min$control@min_t_filter, 1L)
  })

  test_that("time and geo defaults include values in aggregate_data", {
    data(aggregates)
    d_agg <- shape(aggregate_data = aggregates,
        item_data = opinion,
        item_names = "abortion",
        time_name = "year",
        geo_name = "state",
        group_names = c("female", "race3"))
    expect_equal(sort(unique(c(opinion$year, aggregates$year))),
                     d_agg$control@time_filter)
    expect_equal(sort(unique(c(opinion$state, aggregates$state))),
                     d_agg$control@geo_filter)
  })

  test_that("time and geo defaults include disjoint values in aggregate_data", {
    data(aggregates)
    data.table::setDT(aggregates)
    disjoint_geo_time <- data.table::data.table( year = 0, state = "foo", race =
      "white", female = "male",  item = "abortion", n_grp = 10, s_grp = 10)
    disjoint_geo_time <- data.table::rbindlist(list(aggregates, disjoint_geo_time))

    d_disjoint_agg <- shape(aggregate_data = disjoint_geo_time,
      item_data = opinion,
      item_names = "abortion",
      time_name = "year",
      geo_name = "state",
      group_names = c("female", "race3"))
    expect_equal(sort(unique(c(opinion$year, 0))), d_disjoint_agg$control@time_filter)
    expect_equal(sort(unique(c(opinion$state, "foo"))), d_disjoint_agg$control@geo_filter)
  })

  test_that('constant_item defaults to TRUE', {
    min_item_call <- shape(item_data = opinion, item_names = "abortion",
      time_name = "year", geo_name = "state", group_names = "female")
    expect_true(min_item_call$control@constant_item)
  })

  test_that('aggregate_item_names defaults to unique items in aggregate_data', {
    data(aggregates)
    data.table::setDT(aggregates)
    d_agg <- shape(aggregate_data = aggregates,
      item_data = opinion,
      item_names = "abortion",
      time_name = "year",
      geo_name = "state",
      group_names = c("female", "race3"))
    expect_equal(d_agg$control@aggregate_item_names,
                 sort(unique(aggregates[n_grp > 0, item])))
  })

})
