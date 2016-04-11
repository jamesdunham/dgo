check_order <- function(stan_data) {
  assertthat::assert_that(identical(rownames(stan_data@XX), dimnames(stan_data@MMM)[[3]]))
  assertthat::assert_that(identical(dimnames(stan_data@ZZ)[[2]], dimnames(stan_data@XX)[[2]]))
  assertthat::assert_that(identical(names(stan_data@n_vec), stan_data@group_counts$name))
  n_vec_groups <- copy(stan_data@group_counts)[, group := do.call(paste, c(.SD, sep = "_")), .SDcols = c(stan_data@vars$groups)]
  n_vec_groups[, group := do.call(paste, c(.SD, sep = "_x_")), .SDcols = c("group", stan_data@vars$geo_id)]
  assertthat::assert_that(all(n_vec_groups[["group"]] %in% rownames(stan_data@XX)))
  nonmissing_xx_rows <- rownames(stan_data@XX)[rownames(stan_data@XX) %in% n_vec_groups[["group"]]]
}

check_values <- function(stan_data) {
  # XX is an indicator matrix
  XX_values = sort(unique(as.vector(stan_data@XX)))
  assertthat::assert_that(assertthat::are_equal(XX_values, c(0L, 1L)))

  # n_vec is a count
  n_vec_values = unique(as.vector(stan_data@n_vec))
  assertthat::assert_that(all(n_vec_values %% 1 == 0))
  assertthat::assert_that(all(n_vec_values >= 0))

  # s_vec is a count
  s_vec_values = unique(as.vector(stan_data@s_vec))
  assertthat::assert_that(all(s_vec_values %% 1 == 0))
  assertthat::assert_that(all(s_vec_values >= 0))

  # there can be no more successes than trials
  assertthat::assert_that(all(as.vector(stan_data@s_vec) <= as.vector(stan_data@n_vec)))
}

check_dimensions <- function(stan_data) {
  assertthat::assert_that(equal_length(stan_data@n_vec, stan_data@s_vec))
  assertthat::assert_that(all_equal(dim(stan_data@NNl2), as.integer(c(stan_data@T, stan_data@Q, stan_data@Gl2))))
  assertthat::assert_that(all_equal(dim(stan_data@SSl2), as.integer(c(stan_data@T, stan_data@Q, stan_data@Gl2))))
  assertthat::assert_that(all_equal(dim(stan_data@MMM), c(stan_data@T, stan_data@Q, stan_data@G)))
  assertthat::assert_that(all_equal(dim(stan_data@WT), as.integer(c(stan_data@T, stan_data@Gl2, stan_data@G))))
  assertthat::assert_that(all_equal(dim(stan_data@l2_only), c(stan_data@T, stan_data@Q)))
  assertthat::assert_that(all_equal(dim(stan_data@XX), c(stan_data@G, stan_data@P)))
  assertthat::assert_that(all_equal(dim(stan_data@ZZ), c(stan_data@T, stan_data@P, stan_data@H)))
  assertthat::assert_that(all_equal(dim(stan_data@ZZ_prior), c(stan_data@T, stan_data@P, stan_data@H)))
  assertthat::assert_that(not_empty((stan_data@constant_item)))
  assertthat::assert_that(not_empty((stan_data@separate_t)))
}
