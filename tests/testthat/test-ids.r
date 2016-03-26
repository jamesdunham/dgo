context("ids selector")

expect_silent(ids(state, "year", survey))
expect_silent(ids("state", "year", survey))

expect_silent(ids(state, "year", survey))
expect_silent(ids("state", "year", "survey"))

res = ids(state, "year", survey)
expect_is(res, "list")
expect_is(res$geo, "ItemVar")
expect_is(res$time, "ItemVar")
expect_is(res$survey, "ItemVar")

expect_equal(res$geo@.Data, "state")
expect_equal(res$time@.Data, "year")
expect_equal(res$survey@.Data, "survey")

expect_error(ids(a, b, c, foo), "unused argument")
expect_error(ids(geo = c("a", "b"), c, d))
