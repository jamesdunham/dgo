check_order <- function(d) {
  assertthat::assert_that(identical(rownames(d$XX), dimnames(d$MMM)[[3]]))
  assertthat::assert_that(identical(dimnames(d$ZZ)[[2]], dimnames(d$XX)[[2]]))
  assertthat::assert_that(identical(names(d$n_vec), d$group_counts$name))
  n_vec_groups <- d$group_counts %>%
    dplyr::select(one_of(d$vars$groups, d$vars$geo_id)) %>%
    # dplyr::distinct() %>%
    tidyr::unite_("group", d$vars$groups, sep = "_") %>%
    tidyr::unite_("group", c("group", d$vars$geo_id), sep = "_x_") %>%
    dplyr::select_("group") %>%
    unlist() %>%
    unname()
  assertthat::assert_that(all(n_vec_groups %in% rownames(d$XX)))
  nonmissing_xx_rows <- rownames(d$XX)[rownames(d$XX) %in% n_vec_groups]
  # assertthat::assert_that(identical(n_vec_groups, nonmissing_xx_rows))
}
