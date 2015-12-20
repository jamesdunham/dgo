poststratify <- function(group_means, targets, variables, prop_var) {
  # prop_var <- "Prop"
  n <- nrow(group_means)
  props <- dplyr::inner_join(group_means, targets, by = variables)
  if (!identical(n, nrow(props))) {
    warning("Dropped ", n - nrow(props), " strata not found in targets")
  }
  str(props)

  props <- props %>%
    dplyr::group_by_(.dots = variables) %>%
    dplyr::mutate_(weighted_value = ~value * Prop)

  # Proportions within years should sum to 1
  prop_sums <- props %>%
    dplyr::group_by_(.dots = "year") %>%
    dplyr::summarise(prop = sum(Prop))
  # prop is a double
  assertthat::assert_that(is_subset(round(unlist(prop_sums$prop), 4), 1L))

  means <- props %>%
    dplyr::group_by_(.dots = c("geo", "year")) %>%
    dplyr::summarise(weighted_mean = ~sum(weighted_value)) %>%
    dplyr::ungroup()

  return(means)
}
