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
    expect_output(print(res), "dgirt samples from")
    expect_output(summary(res), "dgirt samples from")
    expect_is(summary(res, verbose = TRUE), "list")

    tab <- get_posterior_mean(res)
    expect_named(tab, c(attr(tab, "id_vars"), "mean"))
  })

})
