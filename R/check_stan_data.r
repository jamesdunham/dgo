check_order <- function(d) {
  assertthat::assert_that(identical(rownames(d$XX), dimnames(d$MMM)[[3]]))
  assertthat::assert_that(identical(dimnames(d$ZZ)[[2]], dimnames(d$XX)[[2]]))
  assertthat::assert_that(identical(names(d$n_vec), d$group_counts$name))
  n_vec_groups <- copy(d$group_counts)[, group := do.call(paste, c(.SD, sep = "_")), .SDcols = c(d$vars$groups)]
  n_vec_groups[, group := do.call(paste, c(.SD, sep = "_x_")), .SDcols = c("group", d$vars$geo_id)]
  assertthat::assert_that(all(n_vec_groups[["group"]] %in% rownames(d$XX)))
  nonmissing_xx_rows <- rownames(d$XX)[rownames(d$XX) %in% n_vec_groups[["group"]]]
}

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

check_dimensions <- function(d) {
  assertthat::assert_that(equal_length(d$n_vec, d$s_vec))
  assertthat::assert_that(all_equal(dim(d$NNl2), as.integer(c(d$T, d$Q, d$Gl2))))
  assertthat::assert_that(all_equal(dim(d$SSl2), as.integer(c(d$T, d$Q, d$Gl2))))
  assertthat::assert_that(all_equal(dim(d$MMM), c(d$T, d$Q, d$G)))
  assertthat::assert_that(all_equal(dim(d$WT), as.integer(c(d$T, d$Gl2, d$G))))
  assertthat::assert_that(all_equal(dim(d$l2_only), c(d$T, d$Q)))
  assertthat::assert_that(all_equal(dim(d$XX), c(d$G, d$P)))
  assertthat::assert_that(all_equal(dim(d$ZZ), c(d$T, d$P, d$H)))
  assertthat::assert_that(all_equal(dim(d$ZZ_prior), c(d$T, d$P, d$H)))
  assertthat::assert_that(not_empty((d$constant_item)))
  assertthat::assert_that(not_empty((d$separate_t)))
}
