suppressMessages({
  library(data.table)
  context("dgirtfit class")

  suppressWarnings({
    res <- dgirt(toy_dgirt_in, iter = 5, chains = 1, seed = 42)
  })

  test_that("dgirt returns class dgirt_fit", {
    expect_s4_class(res, "dgirt_fit")
    expect_s4_class(res, "dgo_fit")
    expect_true(inherits(res, "stanfit"))
  })

  test_that("dgmrp returns class dgmrp_fit/dgo_fit/stanfit", {
    suppressMessages({
      dgmrp_in <- shape(opinion, item_names = "abortion", time_name = "year",
        geo_name = "state", group_names = "race3", geo_filter = c("CA", "GA"))
    })
    dgmrp_result <- dgmrp(dgmrp_in, iter = 5, chains = 1, seed = 42)
    expect_s4_class(dgmrp_result, "dgmrp_fit")
    expect_s4_class(dgmrp_result, "dgo_fit")
    expect_true(inherits(dgmrp_result, "stanfit"))
  })

  test_that("dgirt methods work", {
    res <- dgirt(toy_dgirt_in, iter = 5, chains = 1, seed = 42)
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
