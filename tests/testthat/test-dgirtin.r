context("dgirt_in methods")

data(aggregates)
aggregate_items <- unique(aggregates$item)
data.table::setDT(aggregates)
shaped <- suppressMessages(shape(opinion, item_names = "abortion", aggregate_data = aggregates,
  aggregate_item_names = aggregate_items, time_name = "year", geo_name =
    "state", group_names = c("race3", "female")))

test_that("by argument to get_n works", {
  data(toy_dgirt_in)
  expect_named(get_n(toy_dgirt_in, by = "state"), c("state", "n"))
  expect_named(get_n(toy_dgirt_in, by = c("state", "race3")), c("state", "race3", "n"))
})

test_that("aggregate_name argument to get_n works", {
  expect_named(get_n(shaped, aggregate_name = "state"), c("state", "n"))
  expect_error(get_n(toy_dgirt_in, aggregate_name = "state"),
    "no aggregate data")
})

test_that("by argument to get_item_n works", {
  data(toy_dgirt_in)
  item_names <- c("affirmative_action", "gaymarriage_amendment")
  expect_named(get_item_n(toy_dgirt_in, by = "state"), c("state", item_names))
  expect_named(get_item_n(toy_dgirt_in, by = c("state", "race3")), c("state",
      "race3", item_names))
})

test_that("by argument to get_item_n works with aggregate data", {
  expect_true("year" %in% names(get_item_n(shaped, aggregate_data = TRUE, by =
        "year")))
  expect_true(all(c("year", "race3") %in% names(get_item_n(shaped,
          aggregate_data = TRUE, by = c("year", "race3")))))
})

test_that("summary, print  and show give expected output", {
  expect_true(any(grepl("Respondents", capture.output(summary(shaped)))))
  expect_true(any(grepl("Items", capture.output(summary(shaped)))))
  expect_true(any(grepl("Respondents", capture.output(print(shaped)))))
  expect_true(any(grepl("Items", capture.output(print(shaped)))))
  expect_true(any(grepl("Respondents", capture.output(show(shaped)))))
  expect_true(any(grepl("Items", capture.output(show(shaped)))))
})

test_that("get_item_names gives expected output", {
  ret = get_item_names(shaped)
  expect_true(identical(names(ret), c('item_data', 'aggregate_data')))
  nonmissing_aggregate_items = sort(unique(aggregates[n_grp > 0][['item']]))
  expect_true(all(nonmissing_aggregate_items %in% ret$aggregate_data))
  expect_true(identical(ret$item_data, 'abortion'))
})

