source("setup.r")

d_min <- min_item_call()
d_mod <- min_modifier_call()

context("validation with valid_names")

toy_table = data.frame(a = 1, b = 2)
Ctrl <- setClass("Ctrl", slots = list("valid_name" = "character",
  "valid_names" = "character", "invalid_name" = "character",
  "invalid_names" = "character", "partial" = "character",
  "dupe_names" = "character", "missing_name" = "character",
  "null_name" = "character"))

ctrl <- new("Ctrl",
            valid_name = "a",
            valid_names = c("a", "b"),
            invalid_name = "x",
            invalid_names = c("x", "y"),
            partial = c("a", "x"),
            dupe_names = c("a", "a"),
            missing_name = "",
            null_name = character(0))

test_that("valid_names works with an S4 and length requirement", {
  is_name <- dgo:::valid_names(toy_table, ctrl, 1)
  expect_silent(is_name("valid_name"))
  expect_error(is_name("valid_names"), ".*should be length 1, not 2.*")
  expect_error(is_name("invalid_name"), ".*should give a variable name in.*")
  expect_error(is_name("invalid_names"), ".*should be length 1, not 2.*")
  expect_error(is_name("partial"), ".*should be length 1, not 2.*")
  expect_error(is_name("dupe_names"), ".*should be length 1, not 2.*")
  expect_error(is_name("missing_name"), ".*should give a variable name in.*")
  expect_error(is_name("null_name"), ".*is required when using.*")
})

test_that("valid_names works with an S4 and no length requirement", {
  are_names <- dgo:::valid_names(toy_table, ctrl)
  expect_silent(are_names("valid_name"))
  expect_silent(are_names("valid_names"))
  expect_error(are_names("invalid_name"), ".*should give variable names in.*")
  expect_error(are_names("invalid_names"), ".*should give variable names in.*")
  expect_error(are_names("partial"), ".*should give variable names in.*")
  expect_error(are_names("dupe_names"), ".*should give unique names")
  expect_error(are_names("missing_name"), ".*should give variable names in.*")
  expect_error(are_names("null_name"), ".*is required when using.*")
})

test_that("valid_names works with plain character names and a length requirement", {
  toy_table = data.frame(a = 1, b = 2)
  is_name <- dgo:::valid_names(toy_table, NULL, 1)
  expect_silent(is_name("a"))
  expect_silent(is_name(c("a", "b")))
  expect_error(is_name("z"), ".*should give a variable name in.*")
  expect_error(is_name(2), ".*should give a variable name in.*")
})

test_that("valid_names works with plain character names and no length requirement", {
  are_names <- dgo:::valid_names(toy_table, NULL, 0)
  expect_silent(are_names(c("a", "b")))
  expect_error(are_names(c("a", "z")), ".*should give variable names in.*")
})

context("required arguments for modifier_data")

test_that("either modifier_names or t1_modifier_names is required", {
  expect_silent(suppressMessages(shape(opinion,
                      modifier_data = states,
                      t1_modifier_names = "prop_evangelicals",
                      item_names = "abortion",
                      time_name = "year",
                      geo_name = "state",
                      group_names = "female")))
  expect_silent(suppressMessages(shape(opinion,
                      modifier_data = states,
                      modifier_names = "prop_evangelicals",
                      item_names = "abortion",
                      time_name = "year",
                      geo_name = "state",
                      group_names = "female")))
  expect_error(suppressMessages(shape(opinion,
                      modifier_data = states,
                      item_names = "abortion",
                      time_name = "year",
                      geo_name = "state",
                      group_names = "female")),
               paste("Either \"modifier_names\" or \"t1_modifier_names\" is required",
               "when using modifier data"))
})

context("required arguments for target_data")

  expect_error(shape(opinion,
                     target_data = targets,
                     proportion_name = "proportion",
                     item_names = "abortion",
                     time_name = "year",
                     geo_name = "state",
                     group_names = "female",
                     survey_name = "source",
                     weight_name = "weight"),
               "raking.*required when using.*target_data")
