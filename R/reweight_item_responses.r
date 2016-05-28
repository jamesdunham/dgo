weight <- function(item_data, target_data, control) {
  # Create individual survey weights from population targets
  #
  # We stop if strata in the sampled data aren't a subset of those in the
  # population targets, or if there's missingness in the stratifying variables.

  if (!length(target_data)) {
    return(item_data)
  }

  target_data <- data.table::setDT(data.table::copy(target_data))

  weight_formulas <- create_formulas(control@strata_names)
  weight_vars <- get_weight_vars(control@strata_names)

  check_levels(item_data, target_data, weight_vars)

  item_data[, c("stratum") := make_stratum(item_data, weight_vars)]
  target_data[, c("stratum") := make_stratum(target_data, weight_vars)]

  # We'll create a design object from the target data.frame; this is a
  # data.frame with attributes that indicate the survey design
  target_design <- survey::svydesign(ids = ~1, data = target_data,
    weights = formula(paste0("~", control@target_proportion_name)))

  item_data[, c("preweight") := rake_weight(item_data,
                                            formulas = weight_formulas,
                                            target_design = target_design,
                                            control = control)]

  item_data[, c("preweight_new") := list(get("preweight") /
                                         mean(get("preweight"), na.rm = TRUE)),
            by = eval(control@time_name)]

  message()
  message("Reweighted item data using target data.")

  item_data
}

create_formulas <- function(strata_names) {
  assertthat::not_empty(strata_names)
  if (is.list(strata_names)) {
    out <- sapply(strata_names, function(x) {
      as.formula(paste("~", paste(x, collapse = " + ")))
    })
  } else {
    out <- list(as.formula(paste("~", paste(strata_names, collapse = " + "))))
  }
  return(out)
}

make_stratum <- function(tab, weight_vars) {
  interaction(tab[, weight_vars, with = FALSE], drop = TRUE)
}

get_weight_vars <- function(strata_names) {
  unique(unlist(strata_names))
}

rake_weight <- function(item_data, formulas, target_design, control) {
  ds <- survey::svydesign(ids = ~1, data = item_data,
                          weights = formula(paste0("~", control@weight_name)))
  # NB this weight variable is the individual weight
  pop_list <- lapply(formulas, survey::svytable, design = target_design)
  rk <- rake_partial(design = ds, sample.margins = formulas,
                     population.margins = pop_list)
  wts <- 1 / rk$prob
  return(wts)
}


check_levels <- function(item_data, target_data, weight_vars) {
  for (s in weight_vars) {
    # We can't have missingness in the stratifying variables of the sampled data
    if (!assertthat::noNA(item_data[[s]])) {
      stop("values of '", s, "' in item data include NA")
    }
    # All strata_names in the data must appear in the target matrix. expand.grid
    # errored out, so do this, though it takes a while
    class_match <- identical(class(item_data[[s]]), class(target_data[[s]]))
    if (!class_match) {
      stop("class of '", s, "' differs between item data (", class(item_data[[s]]), ") and target data (",
        class(target_data[[s]]), ")")
    }
    values_match <- is_subset(item_data[[s]], target_data[[s]])
    if (!values_match) {
      stop("values of '", s, "' in item data missing from target data: ", paste(setdiff(item_data[[s]], target_data[[s]]),
          collapse = ", "))
    }
  }
}
