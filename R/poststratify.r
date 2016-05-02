#' \code{poststratify}: poststratify \code{dgirt} estimates
#'
# TODO: expand
#'
#' @param estimates A table giving estimates for groups within strata.
#'
#' @param estimate_names The columns in \code{estimates} to poststratify.
#'
#' @param target_data A table giving the population proportions of groups within
#' strata.
#'
#' @param group_names The names of the columns in \code{estimates} and
#' \code{target_data} that identify groups.
#'
#' @param strata_names The names of the columns in \code{estimates} and
#' \code{target_data} that identify population strata.
#'
#' @param prop_name The name of the column in \code{target_data} that gives
#' population proportions.
#'
#' @param check_sums Optionally, the names of the columns within whose
#' combinations population proportions should sum to one, or an error will
#' occur.
#'
#' @return A table giving poststratified estimates for each stratum.
#'
#' @export
# TODO: poststratify method for dgirtFit
poststratify <- function(estimates,
                         estimate_names,
                         target_data,
                         group_names,
                         strata_names = c('year', 'state'),
                         prop_name = 'proportion',
                         check_sums = NULL) {

  estimates <- data.table::setDT(data.table::copy(estimates))
  targets <- data.table::setDT(data.table::copy(target_data))

  vapply(c(group_names, strata_names), check_target_levels, FUN.VALUE = logical(1L),
         estimates, targets)

  targets_n <- nrow(unique(targets[, c(strata_names, group_names), with = FALSE]))
  if (nrow(targets) > targets_n) {
    warning("There are more rows of proportions in the target data ",
            "than there are combinations of strata and grouping ",
            "variables. Summing proportions over other variables.")

    targets[, setdiff(names(targets), c(strata_names, group_names, prop_name)) := NULL, with = FALSE]
    targets <- targets[, lapply(.SD, sum), .SDcols = prop_name, by = c(strata_names, group_names)]
  }

  estimates_n <- nrow(estimates)
  props <- merge(estimates, targets, all = FALSE, by = c(strata_names, group_names))
  if (!identical(estimates_n, nrow(props))) {
    warning("Dropped ", estimates_n - nrow(props), " group means not found in targets")
  }

  if (length(check_sums)) check_proportions(props, prop_name, check_sums)

  props <- scale_props(props, prop_name, strata_names, check_sums)
  means <- props[, lapply(.SD, function(k) sum(k * props$scaled_prop)),
                 by = strata_names, .SDcols = estimate_names]
  means
}

scale_props <- function(props, prop_name, strata_names, check_sums) {
  strata_sums <- props[, list(strata_sum = sum(get("proportion"))), by = strata_names]
  props <- merge(props, strata_sums, all = FALSE, by = strata_names)
  props[, c("scaled_prop") := get("proportion") / get("strata_sum")]
  props
}

check_proportions <- function(tabular, prop_name, check_sums) {
  prop_sums <- tabular[, lapply(.SD, sum), .SDcols = prop_name, by = check_sums]
  if (!isTRUE(all.equal(1L, unique(prop_sums[[prop_name]]))))
    stop("not all proportions sum to 1 within ", paste(check_sums, collapse = ", "))
  else TRUE
}

check_target_levels <- function(variable, estimates, targets) {
  if (!identical(class(estimates[[variable]]), class(targets[[variable]]))) {
    stop("'", variable, "' inherits from '", class(estimates[[variable]]),
      "' in estimates and '", class(targets[[variable]]), "' in targets")
  }
  else if (!all(unique(estimates[[variable]] %in% targets[[variable]]))) {
    stop("not all values of '", variable, "' in estimates are values of '", variable, "' in targets: ",
      paste(sort(setdiff(estimates[[variable]], targets[[variable]])), collapse = ", "))
  }
  else TRUE
}
