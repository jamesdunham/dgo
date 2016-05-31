suppressMessages({
  context("dgirtfit class")

  res <- suppressWarnings(dgirt(toy_dgirt_in, iter = 5, chains = 1,
                                seed = 42))
  test_that("dgirt returns class dgirtfit", {
    expect_s4_class(res, "dgirtfit")
    expect_true(inherits(res, "stanfit"))
  })

  test_that("dgirt methods work", {
    res <- suppressWarnings(dgirt(toy_dgirt_in, iter = 5, chains = 1,
                                  seed = 42))
    # NOTE: this doesn't work inside devtools::test
    # expect_output(print(res), "Samples were drawn")
    expect_silent(summary(res))
    expect_silent(extract(res))
    expect_silent(get_posterior_mean(res))
  })

  test_that("pars takes a single parameter name", {
  expect_error(as.data.frame(toy_dgirtfit, pars = c("theta_bar", "xi")),
               "\"pars\" should be a single parameter name")
  })

})
