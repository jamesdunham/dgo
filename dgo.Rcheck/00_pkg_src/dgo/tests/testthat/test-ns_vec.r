source("setup.r")
suppressMessages({

  context("n/s vectors")

  test_that("counts for minimal call haven't changed", {
    d_min <- shape(item_data = dgo::opinion,
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
                 item_data = dgo::opinion,
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

  test_that("unobserved groups are added to group counts", {
    d <- shape(item_data = dgo::opinion,
               time_filter = c(0, 2006),
               item_names = "Q_cces2006_abortion",
               time_name = "year",
               geo_name = "state",
               group_names = "female",
               survey_name = "source",
               weight_name = "weight")
    expect_equal(unique(d$group_counts$year), c(0, 2006))
  })

  test_that("n_vec and s_vec have expected order", {
    d_agg <- shape(aggregate_data = aggregates,
                   item_data = dgo::opinion,
                   item_names = "Q_cces2006_abortion",
                   modifier_data = dgo::states,
                   modifier_names = "prop_evangelicals",
                   t1_modifier_names = "income_percapita",
                   time_name = "year",
                   geo_name = "state",
                   group_names = "female",
                   survey_name = "source",
                   weight_name = "weight",
                   time_filter = c(2006:2008),
                   aggregate_item_names = "hlthcare_binary",
                   standardize = TRUE)
    ctrl <- d_agg$control
    n_vec_names <- as.data.table(tstrsplit(names(d_agg$n_vec), "__"))
    colnames(n_vec_names) <- c(ctrl@time_name, ctrl@geo_name, ctrl@group_names,
                               "item")
    n_vec_names[, year := type.convert(year)]

    # we loop over t, q, and g in that order; time should vary slowest
    t_order <- rep(ctrl@time_filter, each = d_agg$G * d_agg$Q)
    expect_equal(t_order, n_vec_names[[ctrl@time_name]])

    # within each t iterate over q
    q_order <- rep(d_agg$gt_items, each = d_agg$G, times = d_agg$T)
    expect_equal(q_order, n_vec_names[["item"]])

    # within each q iterate over g; group levels are the interaction of geo
    # levels and demographic levels, and geo varies fastest, iterated over for
    # each demographic level
    group_levels <- unique(c(d_agg$item_data[[ctrl@group_names]],
                             d_agg$aggregate_data[[ctrl@group_names]]))
    geo_order <- rep(ctrl@geo_filter, times = d_agg$T * d_agg$Q *
                     length(group_levels))
    expect_equal(geo_order, n_vec_names[[ctrl@geo_name]])
    g_order <- rep(group_levels, each = length(ctrl@geo_filter),
                   times = d_agg$Q * d_agg$T)
    expect_equal(g_order, n_vec_names[[ctrl@group_names]])
  })

})
