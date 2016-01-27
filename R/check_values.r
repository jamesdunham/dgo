check_values <- function(d) {
  # XX is an indicator matrix
  XX_values = sort(unique(as.vector(d$XX)))
  assertthat::assert_that(assertthat::are_equal(XX_values, c(0L, 1L)))

  # n_vec is a count
  n_vec_values = unique(as.vector(d$n_vec))
  assertthat::assert_that(all(n_vec_values %% 1 == 0))
  assertthat::assert_that(all(n_vec_values >= 0))

  # s_vec is a count
  s_vec_values = unique(as.vector(d$s_vec))
  assertthat::assert_that(all(s_vec_values %% 1 == 0))
  assertthat::assert_that(all(s_vec_values >= 0))

  # there can be no more successes than trials
  assertthat::assert_that(all(as.vector(d$s_vec) <= as.vector(d$n_vec)))
}
