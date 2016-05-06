context("poststratification")
library(data.table)

test_that("dispatch (probably) works", {
  data(targets)
  expect_silent(suppressWarnings(poststratify(toy_dgirtfit,
                                              target_data = targets,
                                              pars = 'theta_bar',
                                              strata_names = c('year', 'state'),
                                              prop_name = 'proportion',
                                              aggregate = TRUE)))
  estimates <- as.data.frame(t(as.data.frame(toy_dgirtfit, par = 'theta_bar'))[, 1])
  estimates <- expand_rownames(estimates, time_name = "year", geo_name = "state", group_names = "race")
  expect_silent(suppressWarnings(poststratify(estimates,
                                              target_data = targets,
                                              group_names = "race",
                                              strata_names = c("year", "state"),
                                              aggregate = TRUE)))
})

test_that("poststratify works", {
  data(warpbreaks)
  target_data <- warpbreaks[, c("wool", "tension")]
  target_data <- setDT(target_data)[, .N, by = c('wool', 'tension')]
  target_data[, prop := N / sum(N)]
  x = warpbreaks
  x = setDT(x)[!duplicated(x[, list(wool, tension)])]

  tapply_res <- tapply(x$breaks, x$wool, mean)
  res <- poststratify(x, target_data = target_data, group_names = "tension",
                      strata_names = "wool", "prop")
  expect_equivalent(res[["breaks"]], as.vector(tapply_res))

  target_data$prop[c(1,2,4,5)] <- c(1/3, 0, 1/3, 0)
  res <- poststratify(x, target_data = target_data, group_names = "tension",
                      strata_names = "wool", "prop")
  expect_equivalent(weighted.mean(x$breaks[1:3], c(2, 0, 1)), res$breaks[1])
  expect_equivalent(weighted.mean(x$breaks[4:6], c(2, 0, 1)), res$breaks[2])
})

test_that("poststratify can aggregate over grouping and geograhpic variables", {
  data(toy_dgirtfit)
  expect_silent(poststratify(toy_dgirtfit, targets, strata_names = c("state", "year") , aggregate = TRUE))
  expect_silent(poststratify(toy_dgirtfit,
                             targets,
                             strata_names = "state",
                             aggregate = TRUE))
  expect_silent(poststratify(toy_dgirtfit, targets, strata_names = "year" , aggregate = TRUE))
  expect_silent(poststratify(toy_dgirtfit, targets, strata_names = "race" , aggregate = TRUE))
})

test_that("omitted arguments produce errors", {
  
})

test_that("poststratify works for any estimated parameter", {
  for (name in names(index_names)) {
    expect_silent(poststratify(toy_dgirtfit, target_data = targets,
                               strata_names = c("year", "state"),
                               pars = name, aggregate = TRUE))
    samples = as.data.frame(toy_dgirtfit, pars = name)
    expect_silent(poststratify(samples, target_data = targets,
                               strata_names = c("year", "state"),
                               pars = name, aggregate = TRUE))
  }
})
