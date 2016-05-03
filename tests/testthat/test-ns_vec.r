source("setup.r")
suppressMessages({

  context("n/s counts")

  test_that("counts for minimal call haven't changed", {
    d_min <- shape(item_data = dgirt::opinion,
                   item_names = "Q_cces2006_abortion",
                   time_name = "year",
                   geo_name = "state",
                   group_names = "female",
                   survey_name = "source",
                   weight_name = "weight")
    expect_equal_to_reference(d_min$n_vec, "d_min_n_vec.Rds")
    expect_equal_to_reference(d_min$s_vec, "d_min_s_vec.Rds")
  })

  test_that("counts for call with aggregates haven't changed", {
  d_agg <- shape(aggregate_data = aggregates,
                 item_data = dgirt::opinion,
                 item_names = "Q_cces2006_abortion",
                 time_name = "year",
                 geo_name = "state",
                 group_names = "female",
                 survey_name = "source",
                 weight_name = "weight",
                 time_filter = c(2006:2010),
                 aggregate_item_names = "hlthcare_binary")
  expect_equal_to_reference(d_agg$n_vec, "d_agg_n_vec.Rds")
  expect_equal_to_reference(d_agg$s_vec, "d_agg_s_vec.Rds")
  })

})
