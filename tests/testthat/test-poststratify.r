context("poststratification")

test_that("dispatch seems to work", {
  suppressMessages(library(data.table))
  data(toy_dgirtfit)
  data(targets)
  setDT(targets)
  targets <- targets[year %in% 2006:2008,
                     list("proportion" = sum(proportion)),
                     by = c("year", "state", "race")]
  expect_silent(poststratify(toy_dgirtfit,
                             target_data = targets,
                             pars = 'theta_bar',
                             strata_names = c('year', 'state'),
                             aggregated_names = 'race'))

  estimates <- as.data.frame(toy_dgirtfit)
  data(targets)
  setDT(targets)
  targets <- targets[year %in% 2006:2008,
                     list("proportion" = sum(proportion)),
                     by = c("year", "state", "race")]
  expect_silent(poststratify(estimates,
                             targets,
                             strata_names = c("year", "state"),
                             aggregated_names = "race"))
})

test_that("poststratify and weighted.mean results are equivalent", {
  data(warpbreaks)
  target_data <- warpbreaks[, c("wool", "tension")]
  target_data <- setDT(target_data)[, .N, by = c('wool', 'tension')]
  target_data[, prop := N / sum(N)]
  x = warpbreaks
  x = setDT(x)[!duplicated(x[, list(wool, tension)])]
  names(x)[1] <- "value"

  tapply_res <- tapply(x$value, x$wool, mean)
  res <- poststratify(x, target_data = target_data, aggregated_names = "tension",
                      strata_names = "wool", prop_name = "prop")
  expect_equivalent(res[["value"]], as.vector(tapply_res))

  target_data$prop[c(1,2,4,5)] <- c(1/3, 0, 1/3, 0)
  res <- poststratify(x, target_data = target_data, aggregated_names = "tension",
                      strata_names = "wool", prop_name = "prop")
  expect_equivalent(weighted.mean(x$value[1:3], c(2, 0, 1)), res$value[1])
  expect_equivalent(weighted.mean(x$value[4:6], c(2, 0, 1)), res$value[2])
})

test_that("omitted arguments produce errors", {

  data(targets)
  setDT(targets)
  targets <- targets[year %in% 2006:2008,
                     list("proportion" = sum(proportion)),
                     by = c("year", "state", "race")]
  expect_silent(poststratify(toy_dgirtfit,
                             target_data = targets,
                             strata_names = c('year', 'state'),
                             aggregated_names = 'race'))
  
})

test_that("missing variables produce stop", {
  data(targets)
  expect_error(poststratify(toy_dgirtfit, target_data = targets,
                               strata_names = "foo", aggregate = TRUE),
               "foo .* isn't a name in the table of estimates")
  expect_error(poststratify(toy_dgirtfit, target_data = targets[, -1],
                               strata_names = "state", aggregate = TRUE),
               "state .* isn't a name in target_data")
})

test_that("poststratify works for gamma, gamma_raw, and theta_bar", {
  params <- index_names[c("gamma", "gamma_raw", "theta_bar")]
  data(toy_dgirtfit)
  data(targets)
  for (i in seq_along(params)) {
    i_names <- setdiff(params[[i]], "hier_params")
    indexes <- sapply(i_names, function(x)
                      slot(toy_dgirtfit@dgirt_in$control, x))
    if (length(indexes) > 1L) {
      for (index in indexes) {
        data(targets)
        setDT(targets)
        targets <- targets[year %in% 2006:2008,
                           list("proportion" = sum(proportion)),
                           by = indexes]
        expect_silent(
        poststratify(toy_dgirtfit,
                     target_data = targets,
                     strata_names = index,
                     aggregated_names = setdiff(indexes, index),
                     pars = names(params)[i]))
      }
    }
  }
})

test_that("variables duplicated between strata_ and group_names produce a stop", {
  # expect_error(poststratify(toy_dgirtfit, target_data = targets,
  #                              strata_names = "race"))
})
