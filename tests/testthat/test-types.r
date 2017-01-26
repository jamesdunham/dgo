source("setup.r")
suppressMessages({

  context("input types")

  data(opinion)
  data(states)
  
  d_min <- min_item_call()
  d_mod <- min_modifier_call()

  test_that("minimal shape calls are successful", {
    expect_silent(suppressMessages(min_item_call()))
    expect_silent(suppressMessages(min_modifier_call()))
    expect_silent(suppressMessages(min_groupless_call()))
  })

  test_that("stop_if_empty works", {
    x <- data.frame()
    expect_error(stop_if_empty(x), "all dimensions of x should be positive")
  })

  test_that("check_count works", {
    expect_silent(check_count(cars, "speed"))
  })
  
  test_that("check_count catches non-integers", {
    data(cars)
    cars$speed <- cars$speed + 0.5
    expect_error(check_count(cars, "speed"),
                 "values of \"speed\" in cars should be positive integers")
  })

  test_that("check_count catches negative numbers", {
    data(cars)
    cars$speed <- cars$speed * -1
    expect_error(check_count(cars, "speed"),
                 "values of \"speed\" in cars should be positive integers")
  })

  test_that("factor values for geo_name in item_data produce a warning", {
    opinion$state <- as.factor(opinion$state)
    expect_warning(min_item_call(item_data = opinion), "Coercing factor")
  })

  test_that("numeric values for geo_name produce an error", {
    opinion$state <- suppressWarnings(as.numeric(opinion$state))
    expect_error(min_item_call(item_data = opinion), "should be character or factor")
  })

  test_that("factor values for geo_name in modifier_data produce a warning", {
    states$state = as.factor(states$state)
    expect_warning(min_modifier_call(modifier_data = states), "Coercing factor")
  })

  test_that("numeric values for geo_name in modifier data produce an error", {
    states$state = suppressWarnings(as.numeric(states$state))
    expect_error(min_modifier_call(modifier_data = states), "should be character or factor")
  })

  test_that("numeric values for group_names in item_data produce an error", {
    opinion$female <- suppressWarnings(as.numeric(opinion$female))
    expect_error(min_item_call(item_data = opinion), "should be character or factor")
  })

  test_that("factor values for group_names in item_data produce a warning", {
    opinion$female <- as.factor(opinion$female)
    expect_warning(min_item_call(item_data = opinion), "Coercing factor")
  })

  test_that("factor values for just one of group_names in item_data produce a warning", {
    opinion$race <- as.factor(opinion$race)
    expect_warning(shape(opinion,
                         item_names = "abortion",
                         time_name = "year",
                         geo_name = "state",
                         group_names = c("female", "race"),
                         survey_name = "source",
                         weight_name = "weight"),
                   "Coercing factor")
  })

  test_that("item variables can be any type", {
    items <- c("abortion", "affirmative_action", "stemcell_research",
      "gaymarriage_amendment")
    opinion[[items[1]]] <- as.character(opinion[[items[1]]])
    opinion[[items[2]]] <- as.ordered(opinion[[items[2]]])
    opinion[[items[3]]] <- as.logical(opinion[[items[3]]])
    opinion[[items[4]]] <- as.factor(opinion[[items[4]]])
    expect_silent(suppressMessages(min_item_call(item_data = opinion,
          item_names = items)))
  })

  test_that("factor values of survey_name produce a warning", {
    opinion$source <- as.factor(opinion$source)
    expect_warning(min_item_call(item_data = opinion), "Coercing factor")
  })

  test_that("numeric values of survey_name produce an error", {
    opinion$source <- suppressWarnings(as.numeric(opinion$source))
    expect_error(min_item_call(item_data = opinion), "should be character or factor")
  })

  test_that("non-integer values of time_name in item_data produce an error", {
    opinion$year <- opinion$year + 0.5
    expect_error(min_item_call(item_data = opinion), "should be integer")
  })

  test_that("factor values of time_name in item_data produce an error", {
    opinion$year <- as.factor(opinion$year)
    expect_error(min_item_call(item_data = opinion), "should be integer")
  })

  test_that("character values of time_name in item_data produce an error", {
    opinion$year <- as.character(opinion$year)
    expect_error(min_item_call(item_data = opinion), "should be integer")
  })

  test_that("non-integer values of time_name in modifier_data work", {
    states$year <- states$year + 0.5
    expect_error(min_modifier_call(modifier_data = states))
  })

  test_that("factor values of time_name in modifier_data produce a warning", {
    states$year <- as.factor(states$year)
    expect_error(min_modifier_call(modifier_data = states), "should be integer or numeric")
  })

  test_that("character values of time_name in modifier_data produce an error", {
    states$year <- as.character(states$year)
    expect_error(min_modifier_call(modifier_data = states), "should be integer or numeric")
  })

  test_that("character values of weight_name in item_data produce an error", {
    opinion$weight <- suppressWarnings(as.character(opinion$weight))
    expect_error(suppressWarnings(min_item_call(item_data = opinion)), "should be numeric")
  })

  test_that("factor values of weight_name in item_data produce an error", {
    opinion$weight <- suppressWarnings(as.factor(opinion$weight))
    expect_error(min_item_call(item_data = opinion), "should be numeric")
  })

})
