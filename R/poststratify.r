#' \code{poststratify}: post-stratify \code{dgirt} estimates
#'
#' Given estimates for modeled groups and their incidence in population strata
#' of interest (typically defined in part geographically), \code{poststratify}
#' returns estimates for the strata. The estimate for each stratum is the mean
#' over the estimates for the groups it comprises, weighted by the proportion of
#' the stratum population the groups contribute.

#' @param x A \code{data.frame} or \code{dgirtfit} object.
#'
#' @param target_data A table giving the proportions contributed to strata
#' populations by modeled groups.
#'
#' @param group_names The names of the columns in \code{x} and
#' \code{target_data} for grouping variables.
#'
#' @param strata_names The names of the columns in \code{x} and
#' \code{target_data} that define strata.
#'
#' @param prop_name The name of the column in \code{target_data} that gives
#' strata proportions.
#'
#' @param aggregate Whether to sum over multiple observations of strata and
#' grouping variables. 
#'
#' @return A table giving poststratified estimates for each stratum.
#'
#' @export
#' @rdname poststratify
#' @examples
#' #  dgirtfit method
#' poststratify(toy_dgirtfit, targets, strata_names = c("year", "state"), aggregate = TRUE)
#'
#' # method for data.frame
#' x <- get_posterior_mean(toy_dgirtfit, pars = 'theta_bar')
#' poststratify(x, target_data = targets, group_names = "race", strata_names = c("year", "state") , aggregate = TRUE)
#'
#' # this is a parameter indexed t
#' x <- get_posterior_mean(toy_dgirtfit, pars = 'xi')
#' poststratify(x, target_data = targets, group_names = "race", strata_names = c("year", "state") , aggregate = TRUE)
post_generic <- function(x, target_data, strata_names, group_names,
                         prop_name = "proportion", aggregate = FALSE, ...) { 

  estimates <- data.table::setDT(data.table::copy(x))
  if (!length(target_data)) stop("target_data is missing")
  targets <- data.table::setDT(data.table::copy(target_data))

  if (!all(strata_names %in% names(estimates)))
    stop("All variables in strata_names should be variables in estimates.")
  if (!all(strata_names %in% names(target_data)))
    stop("All variables in strata_names should be variables in targets.")

  targets_n <- nrow(unique(targets[, c(strata_names, group_names), with = FALSE]))

  if (nrow(targets) > targets_n) {
    if (isTRUE(aggregate)) {
      targets <- targets[, lapply(.SD, sum), .SDcols = prop_name, by = c(strata_names, group_names)]
    } else {
      stop("There are more observations in the target data than ",
           "combinations of strata and grouping variables. If ",
           "\"aggregate\" is TRUE, their proportions will be summed over. ",
           "But this is not recommended unless they are sure to represent ",
           "partitions of the specified strata.")
    }
  }

  extra_cols <- setdiff(names(targets), c(strata_names, group_names, prop_name))
  if (length(extra_cols)) {
    targets[, c(extra_cols) := NULL, with = FALSE]
  }
  value_col <- setdiff(names(estimates), c(strata_names, group_names, prop_name, "rn"))

  estimates_n <- nrow(estimates)
  props <- merge(estimates, targets, all = FALSE, by = c(strata_names, group_names))
  if (!identical(estimates_n, nrow(props))) {
  warning("Not all estimates could be matched with a population proportion ",
            "using the stratifying and grouping variables. ", estimates_n -
              nrow(props), " will be dropped from the output, ",
            "and this may indicate a larger problem.")
  }

  props <- scale_props(props, prop_name, strata_names, group_names)

  check_proportions(props, strata_names)
  res <- props[, lapply(.SD, function(k) sum(k * .SD$scaled_prop)),
               by = strata_names, .SDcols = c(value_col, "scaled_prop")]
  res[, c("scaled_prop") := NULL, with = FALSE]
  data.table::copy(res)
}

check_estimates <- function(estimates, strata_names, group_names) {
  estimates[, lapply(.SD, sum), by = c(stata_names)]
  res
}

scale_props <- function(props, prop_name, strata_names, group_names) {
  strata_sums <- props[, list(strata_sum = sum(get(prop_name))), by = strata_names]
  props <- merge(props, strata_sums, all = FALSE, by = strata_names)
  props[, c("scaled_prop") := get(prop_name) / get("strata_sum")]
  return(props)
}

check_proportions <- function(tabular, strata_names) {
  prop_sums <- tabular[, lapply(.SD, sum), .SDcols = "scaled_prop", by = strata_names]
  if (!isTRUE(all.equal(rep(1L, nrow(prop_sums)), prop_sums$scaled_prop))) {
    stop("Not all proportions sum to 1 within stratifying variables ", 
         cc_and(strata_names), " even though they should have been ",
         "rescaled. (The mean sum is ", round(mean(prop_sums$scaled_prop), 2L),
         "). This could indicate a problem in joining the estimates and ",
         "targets or be a bug.")
  } else TRUE
}

check_target_levels <- function(variable, estimates, targets) {
  if (!identical(class(estimates[[variable]]), class(targets[[variable]]))) {
    stop("'", variable, "' inherits from '", class(estimates[[variable]]),
      "' in estimates and '", class(targets[[variable]]), "' in targets.",
      "Please reconcile the types.")
  } else if (!all(estimates[[variable]] %in% targets[[variable]])) {
    stop("Not all levels of '", variable, "' in estimates are levels of '",
         variable, "' in targets: ", cc_and(setdiff(estimates[[variable]],
                                                    targets[[variable]])),
         ". The target data should give the population proportion of each ",
         "group represented in the estimates.")
  } else TRUE
}
