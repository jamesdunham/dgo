library(dgirt)

suppressMessages({
  context("dgirtfit class")

  res <- dgirt(toy_dgirt_in, iter = 10, chains = 1)
  test_that("dgirt returns class dgirtfit", {
    expect_s4_class(res, "dgirtfit")
  })

  test_that("dgirt methods work", {
    res <- dgirt(toy_dgirt_in, iter = 10, chains = 1)
    expect_output(show(res), "Samples were drawn using NUTS")
    expect_silent(summary(toy_dgirtfit))
    expect_silent(extract(toy_dgirtfit))
    expect_silent(get_posterior_mean(toy_dgirtfit))
  })

})
