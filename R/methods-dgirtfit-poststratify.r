utils::globalVariables(c("value", "scaled_prop"))

#' \code{poststratify}: reweight and aggregate estimates
#'
#' This function reweights and aggregates estimates from \code{dgirt} for strata
#' defined by modeled variables. The names of each of the model's time,
#' geographic, and demographic grouping variables can be given in either the
#' \code{strata_names} or \code{aggregated_names} argument. The result has
#' estimates for the strata indicated by the \code{strata_names} argument,
#' aggregated over the variables specified in \code{aggregated_names}.
#' \code{poststratify} requires a table given as \code{target_data} with
#' population proportions for the interaction of the variables given in
#' \code{strata_names} and \code{aggregated_names}.
#'
#' @rdname poststratify
#' @param ... Additional arguments to methods.
setGeneric("poststratify", signature = "x",
           function(x, target_data, strata_names, aggregated_names,
                    prop_name = "proportion", single_issue = "F", ...)
             standardGeneric("poststratify"))

#' @param pars Selected parameter names.
#' @export
#' @rdname poststratify 
#' @examples
#' 
#' data(toy_dgirtfit)
#'
#' # the stratifying variables should uniquely identify proportions in the
#' # target data; to achieve this, sum over the other variables
#' targets <- aggregate(proportion ~ state + year + race, targets, sum)
#'
#' # the dgirtfit method of poststratify takes a dgirtfit object, the target
#' # data, the names of variables that define population strata, and the  names
#' # of variables to be aggregated over
#' post <- poststratify(toy_dgirtfit, targets, c("state", "year"), "race")
#' @export
setMethod("poststratify", c("dgirtfit"),
  function(x, target_data, strata_names, aggregated_names,
           prop_name = "proportion", single_issue = "F", pars = "theta_bar") {
    x <- as.data.frame(x, pars = pars)
    callGeneric(x, target_data, strata_names, aggregated_names, prop_name, single_issue)
})

#' @param x A \code{data.frame} or \code{dgirtfit} object.
#' @param target_data A table giving the proportions contributed to strata by
#' the interaction of \code{strata_names} and \code{aggregated_names}.
#' @param strata_names Names of variables whose interaction defines
#' population strata.
#' @param aggregated_names Names of variables to be aggregated over in
#' poststratification. 
#' @param prop_name Name of the column in \code{target_data} that gives
#' strata proportions.
#' @return A table of poststratified estimates.
#' @rdname poststratify
#' @export
setMethod("poststratify", "data.frame",
          function(x, target_data, strata_names, aggregated_names,
                   prop_name = "proportion", single_issue = "F", pars = "theta_bar") {
  assert(is.data.frame(target_data))
  assert(all_strings(strata_names))
  assert(all_strings(strata_names))
  assert(assertthat::is.string(prop_name))
  assert(all_strings(pars))
  assert(all_strings(single_issue))

  x <- data.table::setDT(data.table::copy(x))
  ## CW (16-12-24) - For single issue models, translate output back to response scale 
  # print(single_issue)
  if(single_issue=="T"){
  	x$value <- pnorm(x$value)
  }
  # print(x)
  
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
  res <- props[, list(value = sum(value * scaled_prop)), by = strata_names] 
  res[]
})

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
