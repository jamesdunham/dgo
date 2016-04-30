suppressMessages({

  context("minimal item call")

  test_that("*_vec order hasn't changed", {
    # should be identical to previous shape results
    expect_identical(unname(d_min$n_vec), unname(min_item_n))
    expect_identical(unname(d_min$s_vec), unname(min_item_s))

    # should be identical to previous wrangle results
    expect_identical(unname(d_min$n_vec), unname(wrangle_min_item_n))
    expect_identical(unname(d_min$s_vec), unname(wrangle_min_item_s))
  })

  context("aggregate item call")

  test_that("*_vec order hasn't changed", {
    # should be identical to previous shape results
    d_min <- min_item_call()
    expect_identical(unname(d_min$n_vec), unname(min_item_n))
    expect_identical(unname(d_min$s_vec), unname(min_item_s))

    # should be identical to previous wrangle results
    stop("need new previous wrangle results")
    # d_min <- min_agg_call(group_names = "race",
    #                      time_filter = c(2006:2008, 2010),
    #                      aggregate_item_names = "affrmact_binary",
    #                      item_names = c("Q_cces2006_gaymarriageamendment",
    #                                     "Q_cces2006_abortion"))
    # expect_identical(unname(d_min$n_vec), unname(wrangle_agg_item_n))
    # expect_identical(unname(d_min$s_vec), unname(wrangle_agg_item_s))
  })

})
