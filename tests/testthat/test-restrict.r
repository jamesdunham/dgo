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
})

