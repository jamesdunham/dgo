poststratify <- function(group_means, targets, variables, prop_var) {
  n <- nrow(group_means)
  # TODO: reconcile types before joining
  # TODO: aggregate targets to time-geo
  props <- dplyr::inner_join(group_means, targets, by = variables)
  if (!identical(n, nrow(props))) {
    warning("Dropped ", n - nrow(props), " strata not found in targets")
  }
  # TODO: handle joining

  props <- props %>%
    dplyr::group_by_(.dots = variables) %>%
    dplyr::mutate_(weighted_value = lazyeval::interp(~value * prop, prop =
      as.name(prop_var)))

  prop_sums <- props %>%
    dplyr::group_by_(.dots = variables) %>%
    dplyr::summarise_(proportion = lazyeval::interp(~sum(prop), prop =
      as.name(prop_var)))
  # Proportions within years should sum to 1
  # Round before asserting this because prop is a double float
  assertthat::assert_that(is_subset(round(unlist(prop_sums$proportion), 4), 1L))

  means <- props %>%
    dplyr::group_by_(.dots = c("geo", "year")) %>%
    dplyr::summarise_(weighted_mean = ~sum(weighted_value)) %>%
    dplyr::ungroup()

  return(means)
}
