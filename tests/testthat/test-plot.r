test_that("dgirt_plot works with tabular data", {
  targets <- aggregate(proportion ~ state + year + race, targets, sum)
  samples <- poststratify(toy_dgirtfit, targets, c("state", "year"), "race")

  expect_silent(dgirt_plot(samples, "year", "value"))
  expect_silent(dgirt_plot(samples, "year", "value", "state"))
  expect_silent(dgirt_plot(samples, "year", "value", fun.y = "mean"))

  expect_error(dgirt_plot(samples, "foo", "bar"), "\"foo\" should be a variable name in \"x\"")
  expect_error(dgirt_plot(samples, "year", "bar"), "\"bar\" should be a variable name in \"x\"")
  expect_error(dgirt_plot(samples, "year", "bar"), "\"bar\" should be a variable name in \"x\"")
  expect_error(dgirt_plot(samples, "year", "value", "baz"), "\"baz\" should be a variable name in \"x\"")
})
