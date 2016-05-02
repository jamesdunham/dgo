source("setup.r")
suppressMessages({

  d_min <- min_item_call()
  d_mod <- min_modifier_call()

  context("minimal item call")

  test_that("*_vec order hasn't changed", {
    # should be identical to previous shape results
    expect_identical(unname(d_min$n_vec), unname(min_item_n))
    expect_identical(unname(d_min$s_vec), unname(min_item_s))

    # should be identical to previous wrangle results
    expect_identical(unname(d_min$n_vec), unname(wrangle_min_item_n))
    expect_identical(unname(d_min$s_vec), unname(wrangle_min_item_s))
  })

  context("aggregate item call")

  test_that("*_vec order hasn't changed", {
    # should be identical to previous shape results
    d_min <- min_item_call()
    expect_identical(unname(d_min$n_vec), unname(min_item_n))
    expect_identical(unname(d_min$s_vec), unname(min_item_s))

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

    shape_n_vec <- d_agg$n_vec[-grep("hlthcare_binary", names(d_agg$n_vec))]
    shape_s_vec <- d_agg$s_vec[-grep("hlthcare_binary", names(d_agg$s_vec))]
    expect_identical(unname(shape_n_vec), unname(wr_agg_n_vec[-1]))
    expect_identical(unname(shape_s_vec), unname(wr_agg_s_vec[-1]))
  })

})
