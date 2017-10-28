context('assertions')

test_that('all_equal works', {
  expect_true(all_equal(1:2, 1:2))
  expect_false(all_equal(1, 1:2))
  expect_error(assert(all_equal(1, 1:2)), "1 and 1:2 are not all equal")
})

test_that('equal_length works', {
  expect_true(equal_length(1:2, 1:2))
  expect_false(equal_length(1, 1:2))
  expect_false(equal_length(NA, 1:2))
  expect_error(assert(equal_length(1, 1:2)), "1 and 1:2 have different lengths")
})

test_that('is_subset works', {
  expect_true(is_subset(1:2, 1:2))
  expect_true(is_subset(1, 1:2))
  expect_false(is_subset(1:2, 1))
  expect_false(is_subset(1:2, NA))
  expect_error(assert(is_subset(1:2, 1)), "1:2 is not a subset of 1")
})

test_that('not_empty works', {
  expect_true(not_empty(1:2))
  expect_false(not_empty(c()))
  expect_false(not_empty(data.frame()))
  expect_error(assert(not_empty(data.frame())), "empty dimension in data.frame()")
})

test_that('has_all_names works', {
  expect_true(has_all_names(mtcars, c('mpg', 'cyl')))
  expect_false(has_all_names(mtcars, c('nm', 'cyl')))
  expect_error(assert(has_all_names(mtcars, c('nm', 'cyl'))), 'not all of .* are names in mtcars')
})

test_that('all_strings works', {
  expect_true(all_strings(c('mpg', 'cyl')))
  expect_true(all_strings(list('mpg', 'cyl')))
  expect_false(all_strings(character(0)))
  expect_false(all_strings(c('mpg', '')))
  expect_error(assert(all_strings(c('mpg', ''))), 'not all positive-length strings')
})
