context("dgirtfit class")

data(toy_dgirt_in)
data(opinion)
dgirt_ret <- readRDS('dgirt_ret.Rds')
dgmrp_ret <- readRDS('dgmrp_ret.Rds')

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

  test_that("further arguments pass to rstan via ...", {
    # In tools/test_objects.R, the calls to dgirt() and dgmrp() specify iter = 5
    # and chains = 1
    expect_equal(length(ret@stan_args), 1)
    expect_true(all(sapply(ret@stan_args, function(x) x$iter) == 5))
  })

  test_that("defaults pass to rstan", {
    expect_true(all(sapply(ret@stan_args, function(x) x$init_r) ==
        1L))
  })

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

