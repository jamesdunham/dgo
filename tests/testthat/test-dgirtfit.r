suppressMessages({
  context("dgirtfit class")

  # NOTE: until model is precompiled this test takes too long 
  # res <- dgirt(toy_dgirt_in, iter = 1, chains = 1)
  # test_that("dgirt returns class dgirtfit", {
  #   expect_s4_class(res, "dgirtfit")
  # })

  test_that("dgirt methods work", {
    # NOTE: getting this error even with stan()
    # Error in s$summary : $ operator is invalid for atomic vectors
    expect_is(tryCatch(show(toy_dgirtfit), error = function(e) e), "stanfit")
    expect_silent(summary(toy_dgirtfit))
    expect_silent(extract(toy_dgirtfit))
    expect_silent(get_posterior_mean(toy_dgirtfit))
  })

})
