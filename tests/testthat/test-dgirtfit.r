suppressMessages({
  library(data.table)
  context("dgirtfit class")

  suppressWarnings({
    sink("/dev/null", type = "output")
    res <- dgirt(toy_dgirt_in, iter = 5, chains = 1, seed = 42)
    sink()
  })

  test_that("dgirt returns class dgirtfit", {
    expect_s4_class(res, "dgirtfit")
    expect_true(inherits(res, "stanfit"))
  })

  test_that("dgirt methods work", {
    suppressWarnings({
      sink("/dev/null", type = "output")
      res <- dgirt(toy_dgirt_in, iter = 5, chains = 1, seed = 42)
      sink()
    })
    expect_output(print(res), "dgirt samples from")
    expect_output(summary(res), "dgirt samples from")
    expect_is(summary(res, verbose = TRUE), "list")

    tab <- get_posterior_mean(res)
    expect_named(tab, c(attr(tab, "id_vars"), "mean"))
  })

  test_that("dgirtfit and stanfit as.data.frame methods give the same values", {
    sf <- toy_dgirtfit
    class(sf) <- "stanfit"
    sf <- as.data.frame(sf, "theta_bar")

    # including warmup
    tab <- as.data.frame(toy_dgirtfit, discard = FALSE)
    data.table::setDT(tab)
    data.table::setorderv(tab, c("race3", "state", "year", "iteration"))

    expect_equal(tab$value, unname(unlist(sf)))

    # excluding warmup
    tab <- as.data.frame(toy_dgirtfit)
    data.table::setDT(tab)
    data.table::setorderv(tab, c("race3", "state", "year", "iteration"))

    expect_equal(tab$value, unname(unlist(sf)))
  })

})
