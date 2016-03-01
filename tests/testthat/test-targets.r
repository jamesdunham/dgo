context("targets")

expect_silent({
  res = targets(state_demographics, strata = education, prop = "proportion")
})
expect_is(res, "R6")
expect_is(res$strata, "ItemVar")
expect_is(res$prop, "ItemVar")
expect_identical(res$tbl, state_demographics)
expect_identical(as.character(res$strata), "education")
expect_identical(as.character(res$prop), "proportion")
