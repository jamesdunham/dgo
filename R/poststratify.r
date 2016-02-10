#' Poststratify dgirt group means
#'
#' @param group_means The `theta_bar` element of `dgirt` results; a data.frame.
#' @param targets Table in which rows correspond to the population
#'        strata defined by the combinations of `strata` variables; a data.frame.
#' @param strata Variables in `targets` table that define population strata; a
#'        character vector.
#' @param groups Variables in `group_means` table that give `dgirt` covariates;
#'        a character vector.
#' @param prop_var Variable in `targets` table that gives the population
#'        proportion of each stratum; a length-one character vector.
#' @param summands Optionally, variables within whose combinations the
#'        population proportions in `targets` should sum to one, otherwise an error
#'        will appear; a character vector.
#' @return table of poststratified group means
#' @export
poststratify <- function(group_means, targets, strata = c('year', 'state'),
    groups, prop_var = 'proportion', summands = NULL) {
  assertthat::assert_that(assertthat::not_empty(group_means))
  assertthat::assert_that(assertthat::not_empty(targets))
  assertthat::assert_that(all_strings(strata))
  assertthat::assert_that(all_strings(groups))
  assertthat::assert_that(assertthat::is.string(prop_var))

  targets_n <- nrow(dplyr::distinct_(targets, .dots = c(strata, groups)))
  if (nrow(targets) > targets_n) {
    warning("More rows of proportions than combinations of its strata and grouping variables. ",
      "Summing proportions over other variables.")
    targets = targets %>%
      dplyr::group_by_(.dots = c(strata, groups)) %>%
      dplyr::summarise_(.dots = setNames(list(lazyeval::interp(~sum(prop),
        prop = as.name(prop_var))), prop_var))
  }

  sapply(c(groups, strata), check_factor_levels, group_means = group_means,
    targets = targets)

  group_means_n <- nrow(group_means)
  props <- dplyr::inner_join(group_means, targets, by = c(strata, groups))
  if (!identical(group_means_n, nrow(props))) {
    warning("Dropped ", group_means_n - nrow(props), " group means not found in targets")
  }

  props <- scale_props(props, prop_var, strata, summands)
  means <- props %>%
    dplyr::group_by_(.dots = strata) %>%
    dplyr::mutate_(weighted_value = ~value * scaled_prop) %>%
    dplyr::summarise_(value = ~sum(weighted_value)) %>%
    dplyr::ungroup()

  return(means)
}

scale_props <- function(props, prop_var, strata, summands) {
  strata_sums <- props %>%
    dplyr::group_by_(.dots = strata) %>%
    dplyr::summarise_(strata_sum = lazyeval::interp(~sum(prop), prop = as.name(prop_var)))

  if (length(summands) > 1) check_proportions(props, prop_var, summands)

  props <- props %>%
    dplyr::left_join(strata_sums, by = strata) %>%
    dplyr::mutate_(scaled_prop = lazyeval::interp(~prop / strata_sum, prop = as.name(prop_var)))
  check_proportions(props, "scaled_prop", strata)
  props
}

check_proportions <- function(tabular, prop_var, summands) {
  assertthat::assert_that(assertthat::is.string(prop_var))
  assertthat::assert_that(is.character(summands) && all(nchar(summands) > 0))
  sums <- tabular %>%
    dplyr::group_by_(.dots = summands) %>%
    dplyr::summarise_(proportion = lazyeval::interp(~sum(prop), prop = as.name(prop_var)))
    if (!all(round(unlist(sums$proportion), 4) %in% 1)) {
      stop("not all proportions sum to 1 within ", paste(summands, collapse = ", "))
    }
    else {
      invisible(TRUE)
    }
}

check_factor_levels <- function(variable, group_means, targets) {
  assertthat::assert_that(assertthat::has_name(group_means, variable))
  assertthat::assert_that(assertthat::has_name(targets, variable))
  if (!identical(class(group_means[[variable]]), class(targets[[variable]]))) {
    stop("'", variable, "' inherits from '", class(group_means[[variable]]),
      "' in group_means and '", class(targets[[variable]]), "' in targets")
  }
  if (!all(unique(group_means[[variable]] %in% targets[[variable]]))) {
    stop("not all values of '", variable, "' in group_means are values of '", variable, "' in targets: ",
      paste(sort(setdiff(group_means[[variable]], targets[[variable]])), collapse = ", "))
  }
}
