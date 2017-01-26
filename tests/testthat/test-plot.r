context("plots")

test_that("dgirt_plot and plot can be called", {
  expect_silent(plot(toy_dgirtfit))
  expect_silent(dgirt_plot(toy_dgirtfit))
})


test_that("plots can be amended", {
  expect_silent(dgirt_plot(toy_dgirtfit) %+% ylab("foo"))
})

test_that("plot calls dgirt_plot", {
  p <- plot(toy_dgirtfit)
  dp <- dgirt_plot(toy_dgirtfit)
  expect_equal(p, dp)
})

test_that("plot_rhats handles parameters", {
  expect_error(plot_rhats(toy_dgirtfit, pars = "xi", facet_vars = "state"),
               "not indexed by")
  expect_silent(plot_rhats(toy_dgirtfit, pars = "xi", color_var = "year"))
})

test_that("pars must be a length-one character", {
  expect_error(plot_rhats(toy_dgirtfit, pars = NULL), "not a string")
  expect_error(plot_rhats(toy_dgirtfit, pars = c("xi", "sd_item")),
               "not a string")
})
