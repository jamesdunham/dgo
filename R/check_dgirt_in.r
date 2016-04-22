# TODO: move to s4 class validation

check_order <- function(dgirt_in) {
  assertthat::assert_that(identical(rownames(dgirt_in$XX), dimnames(dgirt_in$MMM)[[3]]))
  assertthat::assert_that(identical(dimnames(dgirt_in$ZZ)[[2]], dimnames(dgirt_in$XX)[[2]]))
  assertthat::assert_that(identical(names(dgirt_in$n_vec), dgirt_in$group_counts$name))
  n_vec_groups <- copy(dgirt_in$group_counts)[, group := do.call(paste, c(.SD, sep = "_")), .SDcols = c(dgirt_in$control@group_names)]
  n_vec_groups[, group := do.call(paste, c(.SD, sep = "_x_")), .SDcols = c("group", dgirt_in$control@geo_name)]
  assertthat::assert_that(all(n_vec_groups[["group"]] %in% rownames(dgirt_in$XX)))
  nonmissing_xx_rows <- rownames(dgirt_in$XX)[rownames(dgirt_in$XX) %in% n_vec_groups[["group"]]]
}

check_values <- function(dgirt_in) {
  # XX is an indicator matrix
  XX_values = sort(unique(as.vector(dgirt_in$XX)))
  assertthat::assert_that(assertthat::are_equal(XX_values, c(0L, 1L)))

  # n_vec is a count
  n_vec_values = unique(as.vector(dgirt_in$n_vec))
  assertthat::assert_that(all(n_vec_values %% 1 == 0))
  assertthat::assert_that(all(n_vec_values >= 0))

  # s_vec is a count
  s_vec_values = unique(as.vector(dgirt_in$s_vec))
  assertthat::assert_that(all(s_vec_values %% 1 == 0))
  assertthat::assert_that(all(s_vec_values >= 0))

  # there can be no more successes than trials
  assertthat::assert_that(all(as.vector(dgirt_in$s_vec) <= as.vector(dgirt_in$n_vec)))
}

check_dimensions <- function(dgirt_in) {
  stopifnot(identical(length(dgirt_in$hier_names), dim(dgirt_in$ZZ)[[2]]))
  assertthat::assert_that(equal_length(dgirt_in$n_vec, dgirt_in$s_vec))
  assertthat::assert_that(all_equal(dim(dgirt_in$NNl2), as.integer(c(dgirt_in$T, dgirt_in$Q, dgirt_in$G_hier))))
  assertthat::assert_that(all_equal(dim(dgirt_in$SSl2), as.integer(c(dgirt_in$T, dgirt_in$Q, dgirt_in$G_hier))))
  assertthat::assert_that(all_equal(dim(dgirt_in$MMM), c(dgirt_in$T, dgirt_in$Q, dgirt_in$G)))
  assertthat::assert_that(all_equal(dim(dgirt_in$WT), as.integer(c(dgirt_in$T, dgirt_in$G_hier, dgirt_in$G))))
  assertthat::assert_that(all_equal(dim(dgirt_in$l2_only), c(dgirt_in$T, dgirt_in$Q)))
  assertthat::assert_that(all_equal(dim(dgirt_in$XX), c(dgirt_in$G, dgirt_in$P)))
  assertthat::assert_that(all_equal(dim(dgirt_in$ZZ), c(dgirt_in$T, dgirt_in$P, dgirt_in$H)))
  assertthat::assert_that(all_equal(dim(dgirt_in$ZZ_prior), c(dgirt_in$T, dgirt_in$P, dgirt_in$H)))
  assertthat::assert_that(not_empty((dgirt_in$constant_item)))
  assertthat::assert_that(not_empty((dgirt_in$separate_t)))
}
