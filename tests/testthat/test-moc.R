context('method of composition')

# Setup
set.seed(1)
n_obs <- 100
n.samps <- 1000
y0 <- rnorm(n_obs)
x0 <- rnorm(n_obs)
samp_df <- data.frame(iteration = rep(seq_len(n.samps), each = n_obs),
  x = rep(x0, n.samps) + rnorm(n_obs * n.samps),
  y = rep(y0, n.samps))
mod_rand <- lm(y ~ x, data = subset(samp_df, iteration == 1))

test_that('moc() basically works', {
  # No error from good input
  expect_silent(moc_result <- moc(samp_df, mod_rand))

  # Output has expected dimensions
  expect_equal(nrow(moc_result), n.samps)
  expect_equal(ncol(moc_result), ncol(samp_df) - 1)
})

test_that('iter_var must exist', {
  expect_error(moc(samp_df, mod_rand, iter_var = ""),
    "iter_var %in% names\\(data\\) is not TRUE")
})

test_that('data must be compatible with model', {
  bad_cols <- samp_df[, -2]
  bad_rows <- samp_df[1:2, ]
  # FIXME: what should happen when 'data' gives an unexpected data.frame?
  moc(bad_cols, mod_rand)
  # Error in model.frame.default(formula = y ~ x, data = data_s, drop.unused.levels
  #  = TRUE) : invalid type (list) for variable 'x'
  moc(bad_rows, mod_rand)
  # Error in eigen(Sigma, symmetric = TRUE) :
  #   infinite or missing values in 'x'
})

test_that('v_fun must be a function that returns a vcov matrix', {
  bad_f <- function(x) {
    return(NULL)
  }
  # FIXME: What should happen when v_fun doesn't return a vcov matrix?
  moc(samp_df, mod_rand, vc_fun = bad_f)
})

