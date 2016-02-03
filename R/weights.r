#' Create individual survey weights from population targets
#'
#' Strata in the sampled data must be a subset of those in the population targets.
#' There can't be missingness in the stratifying variables.
#' @param level1 \code{data.frame} with survey responses at the lowest level of aggregation.
#' @param arg Arguments to `wrangle`
#' @return \code{data_frame} of survey responses with new weights normalized to have mean 1 in each period.
create_weights <- function(level1, arg) {

  assertthat::assert_that(assertthat::not_empty(arg$targets) > 0)
  assertthat::assert_that(length(arg$target_groups) > 0)
  assertthat::assert_that(assertthat::is.string(arg$target_proportion))

  weight_formulas <- create_formulas(arg$target_groups)
  weight_vars <- get_weight_vars(arg$target_groups)

  # We can't have missingness in the stratifying variables of the sampled data
  assertthat::assert_that(assertthat::noNA(level1[, weight_vars]))

  targets <- get_group_props(arg$targets, arg$target_groups, arg$target_proportion)

  # All strata in the data must appear in the target matrix. expand.grid
  # errored out, so do this, though it takes a while
  level1 <- level1 %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))
  arg$targets <- arg$targets %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))
  # TODO: make failure here informative
  assertthat::assert_that(is_subset(level1$stratum, arg$targets$stratum))

  # We'll create a design object from the target data.frame; this is a
  # data.frame with attributes that indicate the survey design
  target_design <- survey::svydesign(ids = ~1, data = arg$targets,
    weights = formula(paste0("~", arg$target_proportion)))

  level1$preweight <- rake_weight(level1, formula.list = weight_formulas,
    target_design = target_design, arg = arg)

  level1 <- level1 %>%
    dplyr::group_by_(.dots = arg$t_var) %>%
    dplyr::mutate_(preweight = ~preweight / mean(preweight, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(level1)
}

get_group_props <- function(tabular, target_groups, prop_var) {
  target_group_sums <- tabular %>%
    dplyr::group_by_(.dots = target_groups) %>%
    dplyr::summarise_(target_groups_sum = lazyeval::interp(~sum(prop), prop = as.name(prop_var)))
  tabular <- tabular %>%
    dplyr::left_join(target_group_sums, by = target_groups) %>%
    dplyr::mutate_(scaled_prop = lazyeval::interp(~prop / target_groups_sum, prop = as.name(prop_var)))
  check_proportions(tabular, "scaled_prop", target_groups)
  tabular
}

rake_weight <- function(level1, formula.list, target_design, arg) {
  ds <- survey::svydesign(ids = ~1, data = level1,
    weights = formula(paste0("~", arg$survey_weight)))
  # NB this weight variable here is the individual weight
  pop.list <- lapply(formula.list, survey::svytable, design = target_design)
  rk <- rake_partial(design = ds, sample.margins = formula.list,
    population.margins = pop.list)
  wts <- 1 / rk$prob
  wts
}


create_formulas <- function(target_groups) {
  assertthat::assert_that(assertthat::not_empty(target_groups))
  if (is.list(target_groups)) {
    out <- sapply(target_groups, function(x) {
      as.formula(paste("~", paste(x, collapse = " + ")))
    })
  } else {
    out <- list(as.formula(paste("~", paste(target_groups, collapse = " + "))))
  }
  return(out)
}

get_weight_vars <- function(target_groups) {
  unique(unlist(target_groups))
}
