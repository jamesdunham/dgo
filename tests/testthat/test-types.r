suppressMessages({

  context("minimal calls")

  test_that("shape runs", {
    expect_silent(suppressMessages(min_item_call()))
    expect_silent(suppressMessages(min_modifier_call()))
  })

  context("geo_name type in item_data")

  test_that("factor values produce a warning", {
    opinion$state <- as.factor(opinion$state)
    expect_warning(min_item_call(item_data = opinion), "Coercing factor")
  })

  test_that("numeric values produce an error", {
    opinion$state <- suppressWarnings(as.numeric(opinion$state))
    expect_error(min_item_call(item_data = opinion), "must be 'character'")
  })

  context("geo_name type in modifier_data")

  test_that("factor values produce a warning", {
    states$state = as.factor(states$state)
    expect_warning(min_modifier_call(modifier_data = states), "Coercing factor")
  })

  test_that("numeric values produce an error", {
    states$state = suppressWarnings(as.numeric(states$state))
    expect_error(min_modifier_call(modifier_data = states), "must be 'character'")
  })

  context("group_names type in item_data")

  test_that("numeric values produce an error", {
    opinion$female <- suppressWarnings(as.numeric(opinion$female))
    expect_error(min_item_call(item_data = opinion), "must be 'character'")
  })


  test_that("factor values produce a warning", {
    opinion$female <- as.factor(opinion$female)
    expect_warning(min_item_call(item_data = opinion), "Coercing factor")
  })

  test_that("factor values in just one group variable produces a warning", {
    opinion$race <- as.factor(opinion$race)
    expect_warning(shape(opinion,
                         item_names = "Q_cces2006_abortion",
                         time_name = "year",
                         geo_name = "state",
                         group_names = c("female", "race"),
                         survey_name = "source",
                         weight_name = "weight"),
                   "Coercing factor")
  })

  context("item_names type")

  test_that("character values produce an error", {
    # character
    opinion$Q_cces2006_abortion <- as.character(opinion$Q_cces2006_abortion)
    expect_error(min_item_call(item_data = opinion),
                 "each item should be an ordered factor or numeric")
  })

  test_that("unordered factor values produce an error", {
    opinion$Q_cces2006_abortion <- as.factor(opinion$Q_cces2006_abortion)
    expect_error(min_item_call(item_data = opinion),
                 "each item should be an ordered factor or numeric")
  })

  test_that("ordered factor values do not produce an error", {
    stop("stack overflow")
    # data(opinion)
    # opinion$Q_cces2006_abortion <- as.ordered(opinion$Q_cces2006_abortion)
    # expect_silent(suppressMessages(min_item_call(item_data = opinion)))
  })

  context("survey_name variable type")

  test_that("factor values produce a warning", {
    opinion$source <- as.factor(opinion$source)
    expect_warning(min_item_call(item_data = opinion), "Coercing factor")
  })

  test_that("numeric values produce an error", {
    opinion$source <- suppressWarnings(as.numeric(opinion$source))
    expect_error(min_item_call(item_data = opinion), "must be 'character'")
  })

  context("time_name type in item_data")

  test_that("non-integer values work", {
    opinion$year <- opinion$year + 0.5
    expect_silent(suppressMessages(min_item_call(item_data = opinion)))
  })

  test_that("factor values produce a warning", {
    opinion$year <- as.factor(opinion$year)
    expect_warning(min_item_call(item_data = opinion), "Coercing factor")
  })

  test_that("character values produce an error", {
    opinion$year <- as.character(opinion$year)
    expect_error(min_item_call(item_data = opinion), "must be 'numeric'")
  })

  context("time_name type in modifier_data")

  test_that("non-integer values work", {
    stop("stack overflow")
    # states$year <- states$year + 0.5
    # expect_silent(suppressMessages(min_modifier_call(modifier_data = states)))
  })

  test_that("factor values produce a warning", {
    states$year <- as.factor(states$year)
    expect_warning(min_modifier_call(modifier_data = states), "Coercing factor")
  })

  test_that("character values produce an error", {
    states$year <- as.character(states$year)
    expect_error(min_modifier_call(modifier_data = states), "must be 'numeric'")
  })

  context("weight_name type")

  test_that("character values produce an error", {
    stop("stack overflow")
  #   opinion$weight <- suppressWarnings(as.character(opinion$weight))
  #   expect_error(suppressWarnings(min_item_call(item_data = opinion)), "must be 'numeric'")
  })
  #
  test_that("factor values produce an error", {
    stop("stack overflow")
    # opinion$weight <- suppressWarnings(as.factor(opinion$weight))
    # expect_error(suppressWarnings(min_item_call(item_data = opinion)), "must be 'numeric'")
  })

})
