source("setup.r")
suppressMessages({

  context("restricting input data")
  
  test_that("filters aren't accepted if too restrictive", { 

    expect_error(new("Control", geo_filter = "AK", time_name = "time", geo_name
        = "state", group_names = "foo", time_filter = 2006:2007),
      "if specified \"geo_filter\" should give at least two local geographic areas")

    expect_error(suppressMessages(min_modifier_call(geo_filter = "AK")),
      "if specified \"geo_filter\" should give at least two local geographic areas")

  })

  context("restricting modifier data")

  test_that("groups unobserved in item_data are dropped from modifier_data", {
    # TODO
  })

  context("standardizing modifier data")

  test_that("standardize argument for modifier_data works", {

    data(states)
    std_res <- min_modifier_call(standardize = TRUE)
    expect_true(std_res$control@standardize)
    expect_equivalent(mean(std_res$modifier_data$prop_evangelicals), 0)
    expect_equivalent(sd(std_res$modifier_data$prop_evangelicals), 1)

    nonstd_res <- min_modifier_call(standardize = FALSE)
    expect_false(nonstd_res$control@standardize)
    expect_equivalent(mean(nonstd_res$modifier_data$prop_evangelicals),
                      mean(states$prop_evangelicals[states$year %in% 2006:2010]))
    expect_equivalent(sd(nonstd_res$modifier_data$prop_evangelicals),
                      sd(states$prop_evangelicals[states$year %in% 2006:2010]))
  })

  context("keeping id_vars in item_data")

  test_that("id_vars are kept in item_data", {
    opinion$respondent = 1:nrow(opinion)
    d_min = min_modifier_call(item_data = opinion, id_vars = "respondent")
    expect_true("respondent" %in% names(d_min$item_data))
  })

  test_that("id_vars can be NULL", {
    expect_silent(suppressMessages(min_modifier_call(id_vars = NULL)))
  })

  test_that("id_vars has to exist if specified", {
    expect_error(min_modifier_call(id_vars = "foo"),
                 "\"id_vars\" should give variable names in \"item_data\"")
  })

  context("disjoint item and modifier groups")

  agg_disjoint <- data.table::data.table(
    race = "white", female = "male", year = 0, item = "abortion",
    state = "foo", n_grp = 1, s_grp = 1)
  agg_disjoint <- data.table::rbindlist(list(aggregates, agg_disjoint))
  agg_disjoint = agg_disjoint[n_grp > 0]
  d_disjoint_agg <- min_agg_call(aggregate_data = agg_disjoint,
                                 aggregate_item_names = unique(agg_disjoint$item))

  test_that("all items in `aggregate_data` are used", {
    expect_equal(d_disjoint_agg$control@item_names,
                 sort(unique("abortion", d_disjoint_agg$item)))
    expect_equal(sort(unique(d_disjoint_agg$group_counts$item)),
                 # "abortion" is given as the `item_names` argument
                 # to min_agg_call()
                 sort(c("abortion_gt1", unique(agg_disjoint$item))))
  })

  test_that("all periods in `aggregate_data` are used by default", {
    expect_equal(d_disjoint_agg$control@time_filter,
                 sort(c(unique(opinion$year), 0)))
    expect_equal(sort(unique(d_disjoint_agg$group_counts[["year"]])), 
                 sort(c(unique(opinion$year), 0)))
  })

  test_that("all geos in `aggregate_data` are used by default", {
    expect_equal(d_disjoint_agg$control@geo_filter,
                 sort(c(unique(opinion$state), "foo")))
    expect_equal(sort(unique(d_disjoint_agg$group_counts[["state"]])), 
                 sort(c(unique(opinion$state), "foo")))
  })

  test_that("periods in `aggregate_data` can be restricted by time_filter", {
    years = 2006:2008
    d_filtered <- min_agg_call(aggregate_data = agg_disjoint,
                               aggregate_item_names = unique(agg_disjoint$item),
                               time_filter = years)
    expect_equal(d_filtered$control@time_filter, years)
    expect_equal(sort(unique(d_filtered$group_counts[["year"]])), years)
  })

  test_that("geo in `aggregate_data` can be restricted by geo_filter", {
    geos  = c("AK", "WI")
    d_filtered <- min_agg_call(aggregate_data = agg_disjoint,
                               aggregate_item_names = unique(agg_disjoint$item),
                               geo_filter = geos)
    expect_equal(d_filtered$control@geo_filter, geos)
    expect_equal(sort(unique(d_filtered$group_counts[["state"]])), geos)
  })

  test_that("all groups in `aggregate_data` are used", {
    disjoint_groups <- data.table::data.table(
      race = "white", female = "other", year = 2006, item = "abortion",
      state = "AK", n_grp = 1, s_grp = 1)
    aggregates <- data.table::rbindlist(list(aggregates, disjoint_groups))
    d_disjoint_groups <- min_agg_call(aggregate_data = aggregates,
                                      aggregate_item_names = unique(aggregates$item))
    expect_equal(sort(unique(d_disjoint_groups$group_counts[["female"]])),
                 c("female", "male", "other"))
  })

  test_that("stop_if_any_na works", {
    d <- data.frame(a = NA, b = 1, stringsAsFactors = FALSE)
    expect_error(stop_if_any_na(d, "a"), "NA values")
    expect_error(stop_if_any_na(d, c("a", "b")), "NA values")
    expect_silent(stop_if_any_na(d, "b"))
  })

})
