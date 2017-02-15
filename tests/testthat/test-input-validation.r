source("setup.r")
suppressMessages({

  context("variation in inputs")

  test_that("no variation in time (a single period) is OK", {
    data(opinion)
    data.table::setDT(opinion)
    expect_silent(suppressMessages({
      d_in = shape(opinion[year == 2006],
             item_names = "abortion",
             time_name = "year",
             geo_name = "state",
             group_names = "female",
             survey_name = "source",
             weight_name = "weight")}))
    expect_silent({
      sink("/dev/null", type = "output")
      d_out = dgirt(d_in, iter = 30, chains = 1)
      as.data.frame(d_out)
      sink()
    })
  })

  test_that("no variation in survey identifier is OK", {
    data(opinion)
    data.table::setDT(opinion)
    data(targets)
    expect_silent(suppressMessages({
      d_in = shape(opinion[source == "CCES_2006"],
             target_data = targets,
             raking = ~ state,
             item_names = "abortion",
             time_name = "year",
             geo_name = "state",
             group_names = "female",
             survey_name = "source",
             weight_name = "weight")}))
    expect_silent({
      sink("/dev/null", type = "output")
      d_out = dgirt(d_in, iter = 30, chains = 1)
      as.data.frame(d_out)
      sink()
    })
  })

  test_that("no variation in geography produces an error", {
    data(opinion)
    data.table::setDT(opinion)
    expect_error(suppressMessages({
      d_in = shape(opinion[state == "MA"],
             item_names = "abortion",
             time_name = "year",
             geo_name = "state",
             group_names = "female",
             survey_name = "source",
             weight_name = "weight")
    }), "state doesn't vary in item_data")
  })

  test_that("no variation in grouping produces an error", {
    data(opinion)
    data.table::setDT(opinion)
    expect_error(suppressMessages({
      d_in = shape(opinion[female == "female"],
             item_names = "abortion",
             time_name = "year",
             geo_name = "state",
             group_names = "female",
             survey_name = "source",
             weight_name = "weight")
    }), "female doesn't vary in item_data")
  })

  test_that("NAs in unused subsets of modifier_data are fine", {
    data(opinion)
    data(states)
    states[states$year == 2008, "income_percapita"] <- NA
    expect_silent(suppressMessages(shape(opinion,
      item_names = "abortion",
      time_name = "year",
      geo_name = "state",
      group_names = "female",
      modifier_data = states,
      modifier_names = "income_percapita",
      time_filter = 2009:2010,
      survey_name = "source",
      weight_name = "weight")))
  })

  test_that("NAs in restricted modifier_data produce an error", {
    data(opinion)
    data(states)
    states[states$year == 2008, "income_percapita"] <- NA
    expect_error(suppressMessages(shape(opinion,
      item_names = "abortion",
      time_name = "year",
      geo_name = "state",
      group_names = "female",
      modifier_data = states,
      modifier_names = "income_percapita",
      time_filter = 2008:2009,
      survey_name = "source",
      weight_name = "weight")),
    "NA values")
  })

  test_that("omitting weight variable is allowed", {
    data(opinion)
    data(states)
    expect_silent(suppressMessages(shape(opinion,
        item_names = "abortion",
        time_name = "year",
        geo_name = "state",
        group_names = "female",
        modifier_data = states,
        modifier_names = "income_percapita",
        time_filter = 2008:2009,
        survey_name = "source")))
  })

  test_that("omitting survey variable is allowed", {
    data(opinion)
    data(states)
    expect_silent(suppressMessages(shape(opinion,
        item_names = "abortion",
        time_name = "year",
        geo_name = "state",
        group_names = "female",
        modifier_data = states,
        modifier_names = "income_percapita",
        time_filter = 2008:2009,
        weight_name = "weight")))
  })


})
