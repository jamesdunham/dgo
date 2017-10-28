fit = suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1))

test_that("compiled model is accessible in stanmodel slot", {
  expect_is(fit@stanmodel, 'stanmodel')
  fit_with_model = suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1,
    model = fit@stanmodel))
  expect_identical(fit@stanmodel, fit_with_model@stanmodel)
})

test_that("model indicated by 'version' must exist", {
  expect_error(
    with_mock(
      stanc = function(...) TRUE,
      stan_model = function(...) TRUE,
      sampling = function(...) fit,
      suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1,
          version = 'foo'))
      ), "should give the name of a model")
})

test_that("version can be a model name or an arbitrary stan file", {
  expect_silent(
    with_mock(
      stanc = function(...) TRUE,
      stan_model = function(...) TRUE,
      sampling = function(...) fit,
      suppressMessages(suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1,
            version = '2017_01_04')))
      ))
  expect_silent(
    with_mock(
      stanc = function(...) TRUE,
      stan_model = function(...) TRUE,
      sampling = function(...) fit,
      suppressMessages(suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1,
            version = 'user-version.stan')))
      ))
})

test_that("pars argument is passed but can be overridden", {
  fit = suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1,
      model = fit@stanmodel))
  expect_equivalent(fit@sim$pars_oi, c(default_pars, 'lp__'))
  fit_with_pars = suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1,
      model = fit@stanmodel, pars = 'theta_bar'))
  expect_equivalent(fit_with_pars@sim$pars_oi, c('theta_bar', 'lp__'))
})

test_that("dgmrp can't take more than one item", {
  expect_error(suppressWarnings(dgmrp(toy_dgirt_in, iter = 1, chains = 1,
        model = fit@stanmodel)), "Multiple items in item data")
})

test_that("version argument is ignored when model is given", {
  fit_with_version = suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 1,
      model = fit@stanmodel, version = "2017_01_04_singleissue"))
  expect_identical(fit@stanmodel, fit_with_version@stanmodel)
})

test_that("init_r defaults to 1L but can be overridden", {
  fit_default = suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 2,
      model = fit@stanmodel))
  expect_true(all(sapply(fit_default@stan_args, `[[`, 'init_r') == 1))
  fit_init_r_2 = suppressWarnings(dgirt(toy_dgirt_in, iter = 1, chains = 2,
      init_r = 2, model = fit@stanmodel))
  expect_true(all(sapply(fit_init_r_2@stan_args, `[[`, 'init_r') == 2))
})

