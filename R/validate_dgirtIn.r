# TODO: move to s4 class validation

check_names <- function(d_in) {
   if (!all.equal(dimnames(d_in$ZZ)[[2]], colnames(d_in$XX)) &&
       all.equal(dimnames(d_in$ZZ_prior)[[2]], colnames(d_in$XX)))
     stop("Names in the design matrix and array of hierarchical data are ",
          "expected to match but don't.")

  n_vec_groups <- data.table::copy(d_in$group_counts)
  n_vec_groups[, `:=`(group = do.call(paste, c(.SD, sep = "__"))),
               .SDcols = c(d_in$control@geo_name,
                           d_in$control@group_names)]
  if (!all(n_vec_groups[["group"]] %in% rownames(d_in$XX))) {
    stop("Not all cell names appear in the rownames of the design matrix, ",
         "which shouldn't be possible.")
  }

  observed_geo = unique(d_in$group_counts[[d_in$control@geo_name]])
  if (sum(!observed_geo %in% dimnames(d_in$ZZ)[[2]]) > 1) {
    stop("Not all geographic areas appear as ZZ columns; this is a bug.")
  }
  if (sum(!observed_geo %in% dimnames(d_in$XX)[[2]]) > 1) {
    stop("Not all geographic areas appear as XX columns; this is a bug.")
  }
  observed_t = unique(d_in$group_counts[[d_in$control@time_name]])
  if (!all(observed_t %in% dimnames(d_in$ZZ)[[1]])) {
    stop("Not all time periods appear as ZZ columns; this is a bug.")
  }
}

check_values <- function(d_in) {
  # XX is an indicator matrix
  XX_values = sort(unique(as.vector(d_in$XX)))
  assertthat::assert_that(assertthat::are_equal(XX_values, c(0L, 1L)))

  # n_vec is a count
  n_vec_values = unique(as.vector(d_in$n_vec))
  assertthat::assert_that(all(n_vec_values %% 1 == 0))
  assertthat::assert_that(all(n_vec_values >= 0))

  # s_vec is a count
  s_vec_values = unique(as.vector(d_in$s_vec))
  assertthat::assert_that(all(s_vec_values %% 1 == 0))
  assertthat::assert_that(all(s_vec_values >= 0))

  # there can be no more successes than trials
  assertthat::assert_that(all(as.vector(d_in$s_vec) <= as.vector(d_in$n_vec)))
}

check_dimensions <- function(d_in) {
  stopifnot(identical(length(d_in$hier_names), dim(d_in$ZZ)[[2]]))
  assertthat::assert_that(equal_length(d_in$n_vec, d_in$s_vec))
  assertthat::assert_that(all_equal(dim(d_in$NNl2), as.integer(c(d_in$T, d_in$Q, d_in$G_hier))))
  assertthat::assert_that(all_equal(dim(d_in$SSl2), as.integer(c(d_in$T, d_in$Q, d_in$G_hier))))
  assertthat::assert_that(all_equal(dim(d_in$WT), as.integer(c(d_in$T, d_in$G_hier, d_in$G))))
  assertthat::assert_that(all_equal(dim(d_in$l2_only), c(d_in$T, d_in$Q)))
  assertthat::assert_that(all_equal(dim(d_in$XX), c(d_in$G, d_in$P)))
  assertthat::assert_that(all_equal(dim(d_in$ZZ), c(d_in$T, d_in$P, d_in$H)))
  assertthat::assert_that(all_equal(dim(d_in$ZZ_prior), c(d_in$T, d_in$P, d_in$H)))
  assertthat::assert_that(not_empty((d_in$constant_item)))
  stopifnot(all.equal(d_in$N, d_in$G * d_in$T * d_in$Q))
}
