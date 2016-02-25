check_order <- function(d) {
  assertthat::assert_that(identical(rownames(d$XX), dimnames(d$MMM)[[3]]))
  assertthat::assert_that(identical(dimnames(d$ZZ)[[2]], dimnames(d$XX)[[2]]))
  assertthat::assert_that(identical(names(d$n_vec), d$group_counts$name))
  n_vec_groups <- d$group_counts %>%
    dplyr::select(one_of(d$vars$groups, d$vars$geo_id)) %>%
    tidyr::unite_("group", d$vars$groups, sep = "_") %>%
    tidyr::unite_("group", c("group", d$vars$geo_id), sep = "_x_") %>%
    dplyr::select_("group") %>%
    unlist() %>%
    unname()
  assertthat::assert_that(all(n_vec_groups %in% rownames(d$XX)))
  nonmissing_xx_rows <- rownames(d$XX)[rownames(d$XX) %in% n_vec_groups]
  # assertthat::assert_that(identical(n_vec_groups, nonmissing_xx_rows))
  TRUE
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
