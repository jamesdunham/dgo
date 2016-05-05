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
#' @return A table giving poststratified estimates for each stratum.
#'
#' @export
#' @examples
#' #  dgirtfit method
#' poststratify(toy_dgirtfit, targets, strata_names = c("year", "state"),
#'                   aggregate = TRUE)
#'
#' # matrix/data.frame method
#' x <- get_posterior_mean(toy_dgirtfit, pars = 'theta_bar')
#' x <- expand_rownames(x, c("state", "race", "year"))
#' poststratify(x, targets, "race", c("year", "state") , aggregate = TRUE)
poststratify.default <- function(x,
                         target_data,
                         group_names,
                         strata_names,
                         prop_name = "proportion",
                         aggregate = FALSE) {

  if (is.matrix(x)) x <- as.data.frame(x)
  estimates <- data.table::setDT(data.table::copy(x))
  targets <- data.table::setDT(data.table::copy(target_data))

  vapply(c(group_names, strata_names), check_target_levels, FUN.VALUE = logical(1L),
         estimates, targets)

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
  iter_col <- setdiff(names(estimates), c(strata_names, group_names, prop_name, "rn"))

  check_estimates(estimates, strata_names, group_names)
  props <- scale_props(targets, prop_name, strata_names)

  estimates_n <- nrow(estimates)
  props <- merge(estimates, props, all = FALSE, by = c(strata_names, group_names))
  if (!identical(estimates_n, nrow(props))) {
    warning("Not all estimates could be matched with a population proportion ",
            "using the stratifying and grouping variables. ", estimates_n -
              nrow(props), " will be dropped from the output, ",
            "and this may indicate a larger problem.")
  }
  check_proportions(props, strata_names)
  res <- props[, lapply(.SD, function(k) sum(k * .SD$scaled_prop)),
               by = strata_names, .SDcols = c(iter_col, "scaled_prop")]
  res[, c("scaled_prop") := NULL, with = FALSE]
  return(res[])
}

check_estimates <- function(estimates, strata_names, group_names) {
  if (any(duplicated(estimates[, c(strata_names, group_names), with = FALSE]))) {
    stop("Combinations of strata and grouping variables are duplicated in ",
         "the provided estimates. For correct rescaling of population ",
         "proportions, the combinations should be unique. Were labels ",
         "extracted successfully (e.g. with `extract_rownames`)?")
  }
}

setGeneric("poststratify", signature = c("x"),
           function(x, ...) standardGeneric("poststratify"))
setMethod("poststratify", c("x" = "data.frame"), poststratify.default)
setMethod("poststratify", c("x" = "matrix"), poststratify.default)

poststratify.dgirtfit <- function(x, target_data, strata_names,
                                  prop_name = "proportion",
                                  pars = "theta_bar", 
                                  aggregate = FALSE) {
    ctrl <- x@dgirt_in$control
    estimates <- t(as.data.frame(x, par = pars))
    id_col <- c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)
    estimates <- expand_rownames(estimates, col_names = id_col)
    res <- poststratify.default(estimates, target_data, ctrl@group_names,
                                strata_names, prop_name, aggregate)
    melted <- data.table::melt(res, id.vars = strata_names, variable.name =
                             "iteration", variable.factor = FALSE)
    melted[, c("iteration") := type.convert(sub("V", "", get("iteration"),
                                              fixed = TRUE)), with = FALSE]
    return(melted)
}

scale_props <- function(targets, prop_name, strata_names) {
  strata_sums <- targets[, list(strata_sum = sum(get(prop_name))), by = strata_names]
  props <- merge(targets, strata_sums, all = FALSE, by = strata_names)
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
      "' in estimates and '", class(targets[[variable]]), "' in targets")
  } else if (!all(estimates[[variable]] %in% targets[[variable]])) {
    stop("not all levels of '", variable, "' in estimates are levels of '",
         variable, "' in targets: ", cc_and(setdiff(estimates[[variable]],
                                                    targets[[variable]])))
  } else TRUE
}
setMethod("poststratify", c("x" = "dgirtfit"), poststratify.dgirtfit)
