context("poststratification")

test_that("dispatch seems to work", {
  data(targets)
  expect_silent(suppressWarnings(poststratify(toy_dgirtfit, pars = 'theta_bar', targets, c('year', 'state'),
                             prop_name = 'proportion', aggregate = TRUE)))

  estimates <- as.data.frame(t(as.data.frame(toy_dgirtfit, par = 'theta_bar'))[, 1])
  estimates = expand_rownames(estimates, c("state", "race", "year"))
  expect_silent(suppressWarnings(poststratify(estimates, targets, "race", c("year", "state"), aggregate = TRUE)))

  test_that("poststratify works", {
    data(warpbreaks)
    target_data <- warpbreaks[, c("wool", "tension")]
    target_data <- setDT(target_data)[, .N, by = c('wool', 'tension')]
    target_data[, prop := N / sum(N)]
    x = warpbreaks
    x = setDT(x)[!duplicated(x[, list(wool, tension)])]

    tapply_res <- tapply(x$breaks, x$wool, mean)
    res <- poststratify.default(x, target_data, "tension", "wool", "prop")
    expect_equivalent(res[["breaks"]], as.vector(tapply_res))

    target_data$prop[c(1,2,4,5)] <- c(1/3, 0, 1/3, 0)
    res <- poststratify.default(x, target_data, "tension", "wool", "prop")
    expect_equivalent(weighted.mean(x$breaks[1:3], c(2, 0, 1)), res$breaks[1])
    expect_equivalent(weighted.mean(x$breaks[4:6], c(2, 0, 1)), res$breaks[2])
  })

  test_that("poststratify can aggregate over grouping and geograhpic variables", {
    data(toy_dgirtfit)
    x <- get_posterior_mean(toy_dgirtfit, pars = 'theta_bar')
    x <- expand_rownames(x, c("state", "race", "year"))
    expect_silent(poststratify(x, targets, "race", "state" , aggregate = TRUE))
  })

})
