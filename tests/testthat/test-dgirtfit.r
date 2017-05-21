context("dgirtfit class")

suppressWarnings({
  data(toy_dgirt_in)
  data(opinion)
  dgirt_ret <- dgirt(toy_dgirt_in, iter = 5, chains = 1, seed = 42)
  dgmrp_in <- shape(opinion, item_names = "abortion", time_name = "year",
    geo_name = "state", group_names = "race3", geo_filter = c("CA", "GA"))
  dgmrp_ret <- dgmrp(dgmrp_in, iter = 5, chains = 1, seed = 42)
})

test_that("dgirt returns class dgirt_fit", {
  expect_s4_class(dgirt_ret, "dgirt_fit")
  expect_s4_class(dgirt_ret, "dgo_fit")
  expect_true(inherits(dgirt_ret, "stanfit"))
})

test_that("dgmrp returns class dgmrp_fit/dgo_fit/stanfit", {
  expect_s4_class(dgmrp_ret, "dgmrp_fit")
  expect_s4_class(dgmrp_ret, "dgo_fit")
  expect_true(inherits(dgmrp_ret, "stanfit"))
})

for (ret in c(dgirt_ret, dgmrp_ret)) {

  test_that("dgirt methods work", {
    expect_output(print(ret), "dgirt samples from")
    expect_output(summary(ret), "dgirt samples from")
    expect_is(summary(ret, verbose = TRUE), "list")
    tab <- get_posterior_mean(ret)
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

}

