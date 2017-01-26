context("item coercion")

test_that("item coercion works", {
  # TODO roll this up into loops
  Ctrl <- setClass("Ctrl", slots = c("item_names"))
  ctrl <- new("Ctrl", item_names = "item")

  # 3-level character
  item_data <- setDT(data.frame("item" = letters[1:3], stringsAsFactors = FALSE))
  expect_warning(result <- coerce_item_types(item_data, ctrl), "will be encoded")
  expect_length(result, 2)
  expect_equal(result[[1]], c(1, 0, 0))
  expect_equal(result[[2]], c(0, 1, 0))

  # 2-level character
  item_data <- setDT(data.frame("item" = letters[1:2], stringsAsFactors = FALSE))
  expect_silent(result <- coerce_item_types(item_data, ctrl))
  expect_length(result, 1)
  expect_equal(result[[1]], c(1, 0))

  # 1-level character
  item_data <- setDT(data.frame("item" = "a", stringsAsFactors = FALSE))
  expect_error(coerce_item_types(item_data, ctrl), "doesn't vary")

  # 3-level factor
  item_data <- setDT(data.frame("item" = letters[1:3]))
  expect_warning(result <- coerce_item_types(item_data, ctrl), "will be encoded")
  expect_length(result, 2)
  expect_equal(result[[1]], c(1, 0, 0))
  expect_equal(result[[2]], c(0, 1, 0))

  # 2-level factor
  item_data <- setDT(data.frame("item" = letters[1:2]))
  expect_silent(result <- coerce_item_types(item_data, ctrl))
  expect_length(result, 1)
  expect_equal(result[[1]], c(1, 0))

  # 1-level factor
  item_data <- setDT(data.frame("item" = "a"))
  expect_error(coerce_item_types(item_data, ctrl), "doesn't vary")

  # 3-level ordered
  item_data <- setDT(data.frame("item" = ordered(1:3)))
  expect_silent(result <- coerce_item_types(item_data, ctrl))
  expect_length(result, 1)
  expect_equal(result[[1]], 1:3)

  # 2-level ordered
  item_data <- setDT(data.frame("item" = ordered(1:2)))
  expect_silent(result <- coerce_item_types(item_data, ctrl))
  expect_length(result, 1)
  expect_equal(result[[1]], 1:2)

  # 1-level ordered
  item_data <- setDT(data.frame("item" = ordered(1)))
  expect_error(coerce_item_types(item_data, ctrl), "doesn't vary")
  expect_length(result, 1)
  expect_equal(result[[1]], 1:2)

  # 3-level numeric
  item_data <- setDT(data.frame("item" = 1:3))
  expect_silent(result <- coerce_item_types(item_data, ctrl))
  expect_length(result, 1)
  expect_equal(result[[1]], 1:3)
})
