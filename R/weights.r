#' Create individual survey weights from population targets
#'
#' Strata in the sampled data must be a subset of those in the population targets.
#' There can't be missingness in the stratifying variables.
#' @param .level1 \code{data.frame} with survey responses at the lowest level of aggregation.
#' @param .arg Arguments to `format_data`
#' @return \code{data_frame} of survey responses with new weights normalized to have mean 1 in each period.
create_weights <- function(.level1, .arg) {

  weight_formulas <- paste0("~", .arg$t_var) %>%
    paste(.arg$geo_id, sep = " + ") %>%
    paste(.arg$groups, sep = " + ") %>%
    sapply(formula)

  weight_vars <- c(.arg$t_var, .arg$geo_id, .arg$groups)

  # We can't have missingness in the stratifying variables of the sampled data
  stopifnot(all(colSums(is.na(.level1[, weight_vars])) == 0))

  # All strata in the data must appear in the target matrix. expand.grid
  # errored out, so do this, though it takes a while
  .level1 <- .level1 %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))
  .arg$targets <- .arg$targets %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))
  stopifnot(unique(.level1$stratum) %in% unique(.arg$targets$stratum))

  # We'll use a modification of an exsting rake function from the survey
  # package.  We want it to ignore (leave unweighted) empty cells.
  rake_partial <- eval(parse(text = eval(sub("compress = FALSE",
    "compress = FALSE, partial=TRUE", deparse(survey::rake)))))

  # We'll create a design object from the target data.frame; this is a
  # data.frame with attributes that indicate the survey design
  target_design <- survey::svydesign(ids = ~1, data = .arg$targets,
    weights = formula(paste0("~", .arg$group_proportion)))

  rake_weight <- function(.level1, formula.list, target_design) {
    ds <- survey::svydesign(ids = ~1, data = .level1,
      weights = formula(paste0("~", .arg$survey_weight)))
    # NB this weight variable here is the individual weight
    pop.list <- lapply(formula.list, survey::svytable, design = target_design)
    rk <- rake_partial(design = ds, sample.margins = formula.list,
      population.margins = pop.list)
    wts <- 1 / rk$prob
    return(wts)
  }

  .level1$preweight <- rake_weight(.level1, formula.list = weight_formulas,
    target_design = target_design)

  .level1 <- .level1 %>%
    dplyr::group_by_(.dots = .arg$t_var) %>%
    dplyr::mutate_(preweight = ~preweight / mean(new_weight, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(.level1)
}
