poststratify <- function(group_means, targets, strata = c('year', 'state'),
    groups,  prop_var = 'proportion') {
  assertthat::assert_that(assertthat::not_empty(group_means))
  assertthat::assert_that(assertthat::not_empty(targets))
  assertthat::assert_that(all_valid_strings(strata))
  assertthat::assert_that(all_valid_strings(groups))
  assertthat::assert_that(assertthat::is.string(prop_var))

  group_means = group_means %>% dplyr::rename(state = geo)
  n <- nrow(group_means)
  # TODO: reconcile types before joining
  targets <- state_demographics
  head(targets)
  head(group_means)
  group_means$race = as.integer(group_means$race)
  group_means$female = as.integer(group_means$female)
  targets$race = as.integer(targets$race)
  targets$female = as.integer(targets$female)
  groups = c('female', 'race')

  targets_n = nrow(dplyr::distinct_(targets, .dots = c(strata, groups)))
  if (nrow(targets) > targets_n) {
    warning("More rows in object '", deparse(call$targets),
      "' than combinations of its strata and grouping variables. ",
      "Summing proportions over other variables.")
    targets = targets %>%
      dplyr::group_by_(.dots = c(strata, groups)) %>%
      dplyr::summarise_(.dots = setNames(list(lazyeval::interp(~sum(prop),
        prop = as.name(prop_var))), prop_var))
  }

  group_means_n = nrow(group_means)
  props <- dplyr::inner_join(group_means, targets, by = c(strata, groups))
  if (!identical(n, nrow(props))) {
    warning("Dropped ", n - nrow(props), " group means not found in targets")
  }

  # NOTE: proportions should sum to 1 within years
  prop_sums <- props %>%
    # TODO: should be general, not "year"
    dplyr::group_by_(.dots = "year") %>%
    dplyr::summarise_(proportion = lazyeval::interp(~sum(prop), prop =
      as.name(prop_var)))
  assertthat::assert_that(is_subset(round(unlist(prop_sums$proportion), 4), 1L))

  props <- props %>%
    dplyr::group_by_(.dots = strata) %>%
    dplyr::mutate_(weighted_value = lazyeval::interp(~value * prop, prop =
      as.name(prop_var)))

  means <- props %>%
    dplyr::group_by_(.dots = strata) %>%
    dplyr::summarise_(weighted_mean = ~sum(weighted_value)) %>%
    dplyr::ungroup()

  return(means)
}
