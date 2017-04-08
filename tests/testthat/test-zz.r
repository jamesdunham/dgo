source("setup.r")

d_min <- min_item_call()
d_mod <- min_modifier_call(standardize = FALSE)
data(states)
data.table::setDT(states)
states = states[year %in% 2006:2010]

context("hierarchical parameter names")

hier_names <- c(sort(unique(states$state))[-1], "femalemale")

test_that('hier_name and ZZ dimnames match', {
  expect_identical(hier_names, d_mod$hier_names)
  expect_identical(hier_names, dimnames(d_mod$ZZ)[[2]])
  expect_identical(dimnames(d_mod$ZZ)[[3]], "prop_evangelicals")
  expect_identical(dimnames(d_mod$ZZ_prior)[[3]], "prop_evangelicals")
})

context("hierarchical parameter counts")

test_that('hier_names is a P-vector', {
  expect_identical(length(hier_names), d_mod$P)
})

test_that('S is the count of geographic units - 1', {
  geo_observed <- length(unique(d_min$group_grid_t$state))
  expect_identical(d_min$S, geo_observed - 1)
})

context("hierarchical modifier is numeric")

test_that('values in ZZ are also numeric', {
  expect_true(is.numeric(d_mod$ZZ))
})

test_that('modifier values appear in the correct rows of ZZ', {
  zz <- reshape2::acast(states[, .(year, state, prop_evangelicals)],
                        year ~ state ~ "prop_evangelicals",
                        value.var = 'prop_evangelicals', drop = FALSE)
  # omit first hierarchical parameter
  zz <- zz[, -1L, , drop = FALSE]
  expect_identical(zz, d_mod$ZZ[, -dim(d_mod$ZZ)[2], , drop = FALSE])
})

test_that('elements in ZZ corresponding to the grouping variable are zeroed', {
  expect_identical(d_mod$ZZ[, length(hier_names), 1L],
                   setNames(rep(0, length(unique(states$year))), unique(states$year)))
})

context("hierarchical modifier is character")

test_that('character values of modifier are dummied', {
  # in legacy code, gave this error: Error in shape_hierarchical_data(level2,
  # level2_modifiers, group_grid_t,  : non-numeric values in hierarchical data.
  # Factor handling probably failed.  Possible quick fix: omit or manually dummy
  # out any factors in 'level2_modifiers' or 'level2_period1_modif iers'.
  expect_error(min_modifier_call(modifier_names = "region",
                            t1_modifier_names = "region"),
               "should be integer or numeric")
})

test_that('elements in ZZ corresponding to the grouping variable are zeroed', {
  expect_identical(d_mod$ZZ[, length(hier_names), 1L],
                   setNames(rep(0, length(unique(states$year))), unique(states$year)))
})

context("hierarchical modifier variable is factor")

test_that('factor values of modifier are dummied', {
  states$region <- as.factor(states$region)
  expect_error(min_modifier_call(modifier_names = "region",
                            t1_modifier_names = "region"),
               "should be integer or numeric")
})

test_that('ZZ is zeroed appropriately when only t1_modifier_names is given', {

  d_t1_only <- shape(item_data = opinion,
                     item_names = "abortion",
                     time_name = "year",
                     geo_name = "state",
                     group_names = "female",
                     modifier_data = states,
                     t1_modifier_names = "prop_evangelicals")
  expect_true(all(d_t1_only$ZZ == 0))
  expect_identical(hier_names, d_t1_only$hier_names)
  expect_identical(hier_names, dimnames(d_t1_only$ZZ)[[2]])
  expect_identical(dimnames(d_t1_only$ZZ)[[3]], "")
})

test_that('ZZ_prior defaults to ZZ', {

  d_tprime_only <- shape(item_data = opinion,
                     item_names = "abortion",
                     time_name = "year",
                     geo_name = "state",
                     group_names = "female",
                     modifier_data = states,
                     modifier_names = "prop_evangelicals")
  expect_identical(d_tprime_only$ZZ_prior, d_tprime_only$ZZ)
  expect_identical(hier_names, d_tprime_only$hier_names)
  expect_identical(hier_names, dimnames(d_tprime_only$ZZ_prior)[[2]])
})

