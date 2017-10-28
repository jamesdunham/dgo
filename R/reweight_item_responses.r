utils::globalVariables(c("raked_weight", "preweight"))

weight <- function(item_data, target_data, control) {
  # Adjust individual survey weights given population targets

  item_data[, c("preweight") := rake_weights(item_data, target_data, control)]

  if (length(control@max_raked_weight)) {
    item_data[preweight > control@max_raked_weight, preweight :=
      control@max_raked_weight]
  }

  item_data[, c("raked_weight") := list(get("preweight") /
                                         mean(get("preweight"), na.rm = TRUE)),
            by = eval(control@time_name)]

  message("\nOriginal weights:")
  message(paste0(capture.output(summary(item_data[[control@weight_name]])),
                                collapse = "\n"))
  message("\nRaked weights:")
  message(paste0(capture.output(summary(item_data[, raked_weight])),
                                collapse = "\n"))
  item_data
}

rake_weights <- function(item_data, target_data, control) {
  if (!is.list(control@raking)) {
    formulas <- list(control@raking)
  } else {
    formulas <- control@raking
  }
  item_design <- survey::svydesign(ids = ~1, data = item_data,
                          weights = formula(paste0("~", control@weight_name)))
  target_design <- survey::svydesign(ids = ~1, data = target_data,
    weights = formula(paste0("~", control@proportion_name)))
  target_tables <- lapply(formulas, survey::svytable,
                          design = target_design)
  raked_design <- rake_partial(design = item_design,
                               sample.margins = formulas,
                               population.margins = target_tables,
                               control = list(epsilon = 1e-09))
  raked_weights <- 1 / raked_design$prob
  assertthat::assert_that(is.numeric(raked_weights))
  if (any(is.na(raked_weights))) {
    warning("NA values in raked weights")
  } 
  if (any(raked_weights < 0)) {
    warning("Negative values in raked weights")
  }
  return(raked_weights)
}

