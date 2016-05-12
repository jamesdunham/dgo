post_generic <- function(x = data.frame, target_data, strata_names,
                         aggregated_names, estimate_names, prop_name,
                         keep = FALSE) {

  x <- data.table::setDT(data.table::copy(x))
  if (!length(target_data)) stop("target_data is missing")
  targets <- data.table::setDT(data.table::copy(target_data))

  missing_cols <- setdiff(strata_names, names(x))
  missing_msgs <- paste("%c", c("is", "are"), "in strata_names but",
               c("isn't a name in", "aren't names in"),
               c(rep(c("the table of estimates to be poststratified.",
                       "target_data"), each = 2L)))
  if (length(missing_cols)) 
    stop(cn(missing_cols, missing_msgs[1], missing_msgs[2]))
  missing_cols <- setdiff(strata_names, names(target_data))
  if (length(missing_cols))
    stop(cn(missing_cols, missing_msgs[3], missing_msgs[4]))

  targets_n <- nrow(unique(targets[, c(strata_names, aggregated_names), with =
                           FALSE]))

  # TODO: check more carefully than nrow()
  if (!identical(nrow(targets), targets_n)) {
      stop("Variables in aggregated_names should partition the strata ",
           "defined by the interaction of the variables in strata_names ",
           "and aggregated_names, ",
           "but there are more observations in target_data (",
           nrow(targets), ") than combinations ",
           "of strata_names and aggregated_names (", targets_n, "). This error ",
           "will appear if (1) the interaction of more variables than those in ",
           "strata_names and aggregated_names define strata in ",
           "target_data; if so, the solution is to aggregate target_data ",
           "over those variables before passing it to poststratify; ",
           "(2) target_data includes superfluous rows (e.g. time periods not ",
           "represented in the estimates); or (3) target_data does not give ",
           "the population proportion data needed to poststratify estimates ",
           "by strata_names and aggregated_names.")
    }

  extra_cols <- setdiff(names(targets), c(strata_names, aggregated_names,
                                          prop_name))
  if (length(extra_cols)) {
    targets[, c(extra_cols) := NULL, with = FALSE]
  }

  x_n <- nrow(x)
  props <- merge(x, targets, all = FALSE, by = c(strata_names,
                                                 aggregated_names))
  if (!identical(x_n, nrow(props))) {
  warning("Not all estimates could be matched with a population proportion ",
            "using the stratifying and grouping variables. ", x_n -
              nrow(props), " will be dropped from the output, ",
            "and this may indicate a larger problem.")
  }

  props <- scale_props(props, prop_name, strata_names)

  check_proportions(props, strata_names)
  res <- props[, lapply(.SD, function(k) sum(k * .SD$scaled_prop)),
               by = strata_names, .SDcols = c(estimate_names, "scaled_prop")]
  res[, c("scaled_prop") := NULL, with = FALSE]
  data.table::copy(res)
}

check_estimates <- function(estimates, strata_names) {
  estimates[, lapply(.SD, sum), by = c(strata_names)]
  estimates
}

scale_props <- function(props, prop_name, strata_names) {
  strata_sums <- props[, list(strata_sum = sum(get(prop_name))),
                       by = strata_names]
  props <- merge(props, strata_sums, all = FALSE, by = strata_names)
  props[, c("scaled_prop") := get(prop_name) / get("strata_sum")]
  return(props)
}

check_proportions <- function(tabular, strata_names) {
  prop_sums <- tabular[, lapply(.SD, sum), .SDcols = "scaled_prop",
                       by = strata_names]
  if (!isTRUE(all.equal(rep(1L, nrow(prop_sums)), prop_sums$scaled_prop))) {
    stop("Not all proportions sum to 1 within stratifying variables ", 
         cc_and(strata_names), " even though they should have been ",
         "rescaled. (The mean sum is ", round(mean(prop_sums$scaled_prop), 2L),
         "). This could indicate a problem in joining the estimates and ",
         "targets or be a bug.")
  } else TRUE
}

check_target_levels <- function(variable, x, targets) {
  if (!identical(class(x[[variable]]), class(targets[[variable]]))) {
    stop("'", variable, "' inherits from '", class(x[[variable]]),
      "' in x and '", class(targets[[variable]]), "' in targets.",
      "Please reconcile the types.")
  } else if (!all(x[[variable]] %in% targets[[variable]])) {
    x_levels <- setdiff(x[[variable]], targets[[variable]])
    stop("Not all levels of '", variable, "' in estimates are levels of '",
         variable, "' in targets. ", cn_and(x_levels , "%c is ","%c are "),
         "missing. The target data should give the population proportion of each
         ", "group represented in the estimates.")
  } else TRUE
}
