suppressMessages({

  context("restricting input data")
  
  test_that("filters aren't accepted if too restrictive", { 

    expect_error(new("Control", geo_filter = "AK", time_name = "time", geo_name =
                     "state", survey_name = "survey", weight_name = "weight",
                   group_names = "foo", time_filter = 2006:2007),
                 "if specified \"geo_filter\" should give at least two local geographic areas")

    expect_error(new("Control", time_filter = 2007, geo_filter = c("AK", "MO"),
                     time_name = "time", geo_name = "state", survey_name = "survey",
                     weight_name = "weight", group_names = "foo"),
                 "if specified \"time_filter\" should give at least two time periods")

    expect_error(suppressMessages(min_modifier_call(geo_filter = "AK")),
                           "if specified \"geo_filter\" should give at least two local geographic areas")
    expect_error(suppressMessages(min_modifier_call(time_filter = 2006)),
                           "if specified \"time_filter\" should give at least two time periods")
  })

  test_that("NA aren't allowed in modifier variables", {
    data(states)
    states$prop_evangelicals[1] <- NA
    expect_error(min_modifier_call(modifier_data = states),
                 "There are NA values in the \"prop_evangelicals\" variable of the modifier data.")
  })
  test_that("NA aren't allowed in modifier variables", {
    data(states)
    states$income_percapita[1] <- NA
    expect_error(min_modifier_call(modifier_data = states, t1_modifier_names = "income_percapita"),
                 "There are NA values in the \"income_percapita\" variable of the modifier data.")
  })
  test_that("NA aren't allowed in modifier variables", {
    data(states)
    states$state[1] <- NA
    expect_error(min_modifier_call(modifier_data = states),
                 "There are NA values in the \"state\" variable of the modifier data.")
  })
  test_that("NA aren't allowed in modifier variables", {
    data(states)
    states$year[1] <- NA
    expect_error(min_modifier_call(modifier_data = states),
                 "There are NA values in the \"year\" variable of the modifier data.")
  })

  context("standardizing modifier data")

  test_that("standardize argument for modifier_data works", {

    data(states)
    std_res <- min_modifier_call(standardize = TRUE)
    expect_true(std_res$control@standardize)
    expect_equivalent(mean(std_res$modifier_data$prop_evangelicals), 0)
    expect_equivalent(sd(std_res$modifier_data$prop_evangelicals), 1)

    nonstd_res <- min_modifier_call()
    expect_false(nonstd_res$control@standardize)
    expect_equivalent(mean(nonstd_res$modifier_data$prop_evangelicals),
                      mean(states$prop_evangelicals))
    expect_equivalent(sd(nonstd_res$modifier_data$prop_evangelicals),
                      sd(states$prop_evangelicals))

    expect_false(min_item_call()$control@standardize)

  })

  context("disjoint item and modifier groups")

  disjoint_geo_time <- data.table::data.table(
    race = "white", female = "male", year = 0, item = "Q_cces2006_abortion",
    state = "foo", n_grp = 1, s_grp = 1)
  aggregates <- data.table::rbindlist(list(dgirt:::aggregates, disjoint_geo_time))
  d_disjoint_agg <- min_agg_call(aggregate_data = aggregates)

  test_that("all items in `aggregate_data` are used", {
    expect_equal(d_disjoint_agg$control@item_names,
                 sort(unique("Q_cces2006_abortion", d_disjoint_agg$item)))
    expect_equal(sort(unique(d_disjoint_agg$group_counts$item)),
                 # "Q_cces2006_abortion" is given as the `item_names` argument
                 # to min_agg_call()
                 sort(c("Q_cces2006_abortion", unique(aggregates$item))))
  })

  test_that("all periods in `aggregate_data` are used by default", {
    expect_equal(d_disjoint_agg$control@time_filter,
                 sort(c(unique(opinion$year), 0)))
    expect_equal(sort(unique(d_disjiont_agg$group_counts[["year"]])), 
                 sort(c(unique(opinion$year), 0)))
  })

  test_that("all geos in `aggregate_data` are used by default", {
    expect_equal(d_disjoint_agg$control@geo_filter,
                 sort(c(unique(opinion$state), "foo")))
    expect_equal(sort(unique(d_disjiont_agg$group_counts[["state"]])), 
                 sort(c(unique(opinion$state), "foo")))
  })

  test_that("periods in `aggregate_data` can be restricted by time_filter", {
    years  = 2006:2008
    d_filtered <- min_agg_call(aggregate_data = aggregates,
                               time_filter = years)
    expect_equal(d_filtered$control@time_filter, years)
    expect_equal(sort(unique(d_filtered$group_counts[["year"]])), years)
  })

  test_that("geo in `aggregate_data` can be restricted by geo_filter", {
    geos  = c("AK", "MI")
    d_filtered <- min_agg_call(aggregate_data = aggregates,
                               geo_filter = geos)
    expect_equal(d_disjoint_agg$control@geo_filter, geos)
    expect_equal(sort(unique(d_filtered$group_counts[["state"]])), geos)
  })

  disjoint_groups <- data.table::data.table(
    race = "white", female = "other", year = 2006, item = "Q_cces2006_abortion",
    state = "AK", n_grp = 1, s_grp = 1)
  aggregates <- data.table::rbindlist(list(dgirt:::aggregates, disjoint_groups))
  d_disjoint_groups <- min_agg_call(aggregate_data = aggregates)

  test_that("all groups in `aggregate_data` are used", {
    expect_equal(sort(unique(d_disjoint_groups$group_counts[["female"]])),
                 c("female", "male", "other"))
  })

})
