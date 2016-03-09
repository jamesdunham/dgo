context("item_filter selector")

expect_silent({
  res = item_filter(1:9, c("AK", "AL"), 1, 1)
})
expect_identical(res$time, 1:9)
expect_identical(as.character(res$geo), c("AK", "AL"))
expect_identical(res$min_t, 1)
expect_identical(res$min_survey, 1)

# TODO: variable checks must happen when Filter attached to Item, not when ItemVar attached ot Filter
# expect_silent(item_filter())
# expect_error(item_filter(time = "foo"))
# expect_error(item_filter(geo = 1L))
# expect_error(item_filter(min_t = "foo"))
# expect_error(item_filter(min_survey = "foo"))
