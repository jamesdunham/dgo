# x = traceback()
# str(x)
source("setup.r")
suppressMessages({
  context("raking")

  test_that("basic syntax works", {
  expect_silent(suppressMessages(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = ~ state)))
  expect_silent(suppressMessages(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ state, ~ year))))
  expect_silent(suppressWarnings(suppressMessages(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = ~ state + year))))
  expect_silent(suppressWarnings(suppressMessages(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ state + year, ~ race)))))
  expect_silent(suppressWarnings(suppressMessages(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ race)))))
  expect_silent(suppressWarnings(suppressMessages(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = ~ race))))
  expect_silent(suppressWarnings(suppressMessages(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ education, ~ race)))))
  })

  test_that("raking variables must exist", {
  # raking gives a single formula 
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = ~ source),
               "\"source\" is a raking formula term but isn't a variable name in \"target_data\"")
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = ~ proportion),
               "\"proportion\" is a raking formula term but isn't a variable name in \"item_data\"")

  # raking gives a list of formulas
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ source, ~ weight)),
               "\"source\" is a raking formula term but isn't a variable name in \"target_data\"")
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ proportion, ~ weight)),
               "\"weight\" is a raking formula term but isn't a variable name in \"target_data\"")
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ proportion, ~ proportion)),
               "\"proportion\" is a raking formula term but isn't a variable name in \"item_data\"")
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ state, ~ proportion)),
               "\"proportion\" is a raking formula term but isn't a variable name in \"item_data\"")

  # raking gives formulas with operators
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ state + weight)),
               "\"weight\" is a raking formula term but isn't a variable name in \"target_data\"")
  expect_error(min_item_call(target_data = targets,
                              target_proportion_name = "proportion",
                              raking = list(~ state + proportion)),
               "\"proportion\" is a raking formula term but isn't a variable name in \"item_data\"")
  })

})
