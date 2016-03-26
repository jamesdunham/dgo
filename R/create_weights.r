#' Create individual survey weights from population targets
#'
#' Strata in the sampled data must be a subset of those in the population targets.
#' There can't be missingness in the stratifying variables.
#' @param level1 \code{data.frame} with survey responses at the lowest level of aggregation.
#' @return \code{data_frame} of survey responses with new weights normalized to have mean 1 in each period.
weight <- function(item) {

  weight_formulas <- create_formulas(item$targets$strata)
  weight_vars <- get_weight_vars(item$targets$strata)

  check_levels(item, item$targets, weight_vars)

  item$tbl <- item$tbl %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))
  item$targets$targets <- item$targets$targets %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))

  # We'll create a design object from the target data.frame; this is a
  # data.frame with attributes that indicate the survey design
  target_design <- survey::svydesign(ids = ~1, data = item$targets,
    weights = formula(paste0("~", item$targets$prop)))

  rake_weight <- function(item, formula.list, target_design) {
    ds <- survey::svydesign(ids = ~1, data = item$tbl,
      weights = formula(paste0("~", item$weight)))
    # NB this weight variable here is the individual weight
    pop.list <- lapply(formula.list, survey::svytable, design = target_design)
    rk <- rake_partial(design = ds, sample.margins = formula.list,
      population.margins = pop.list)
    wts <- 1 / rk$prob
    return(wts)
  }

  item$tbl$preweight <- rake_weight(item, formula.list = weight_formulas,
    target_design = target_design)

  item$tbl <- item$tbl %>%
    dplyr::group_by_(.dots = item$time) %>%
    dplyr::mutate_(preweight = ~preweight / mean(preweight, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

create_formulas <- function(strata) {
  assertthat::not_empty(strata)
  if (is.list(strata)) {
    out <- sapply(strata, function(x) {
      as.formula(paste("~", paste(x, collapse = " + ")))
    })
  } else {
    out <- list(as.formula(paste("~", paste(strata, collapse = " + "))))
  }
  return(out)
}

get_weight_vars <- function(strata) {
  unique(unlist(strata))
}

check_levels <- function(item, weight_vars) {
  for (s in weight_vars) {
    # We can't have missingness in the stratifying variables of the sampled data
    if (!assertthat::noNA(item$tbl[[s]])) {
      stop("values of '", s, "' in item data include NA")
    }
    # All strata in the data must appear in the target matrix. expand.grid
    # errored out, so do this, though it takes a while
    class_match <- identical(class(item$tbl[[s]]), class(item$targets[[s]]))
    if (!class_match) {
      stop("class of '", s, "' differs between item data (", class(item$tbl[[s]]), ") and target data (",
        class(item$targets[[s]]), ")")
    }
    values_match <- is_subset(item$tbl[[s]], item$targets[[s]])
    if (!values_match) {
      stop("values of '", s, "' in item data missing from target data: ", paste(setdiff(item$tbl[[s]], item$targets[[s]]),
          collapse = ", "))
    }
  }
}
