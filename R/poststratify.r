#' Poststratify dgirt group means
#'
#' @param group_means The `theta_bar` element of `dgirt` results; a data.frame.
#' @param targets Table in which rows correspond to the population
#'        strata defined by the combinations of `strata_names` variables; a data.frame.
#' @param strata_names Variables in `targets` table that define population strata; a
#'        character vector.
#' @param groups Variables in `group_means` table that give `dgirt` covariates;
#'        a character vector.
#' @param prop_var Variable in `targets` table that gives the population
#'        proportion of each stratum; a length-one character vector.
#' @param summands Optionally, variables within whose combinations the
#'        population proportions in `targets` should sum to one, otherwise an error
#'        will appear; a character vector.
#' @return table of poststratified group means
#' @export

# TODO: poststratify method for dgirtFit
# poststratify(group_means, state_targets, "race")
# FIXME: estimates renamed as V1
poststratify <- function(group_means,
                         target_data,
                         group_names,
                         strata_names = c('year', 'state'),
                         prop_name = 'proportion',
                         check_sums = NULL) {

  group_means <- setDT(copy(group_means))
  targets <- setDT(copy(target_data))

  sapply(c(group_names, strata_names), check_levels,
         group_means = group_means,
         targets = targets)

  targets_n <- nrow(unique(targets[, c(strata_names, group_names), with = FALSE]))
  if (nrow(targets) > targets_n) {
    warning("There are more rows of proportions in the target data ",
            "than there are combinations of strata and grouping ",
            "variables. Summing proportions over other variables.")

    targets[, setdiff(names(targets), c(strata_names, group_names, prop_name)) := NULL, with = FALSE]
    targets <- targets[, lapply(.SD, sum), .SDcols = prop_name, by = c(strata_names, group_names)]
  }

  group_means_n <- nrow(group_means)
  props <- merge(group_means, targets, all = FALSE, by = c(strata_names, group_names))
  if (!identical(group_means_n, nrow(props))) {
    warning("Dropped ", group_means_n - nrow(props), " group means not found in targets")
  }

  if (length(check_sums)) check_proportions(props, prop_name, summands)

  props <- scale_props(props, prop_name, strata_names, summands)
  means <- props[, sum(`mean-chain:1` * scaled_prop), by = strata_names]
  means
}

scale_props <- function(props, prop_name, strata_names, summands) {
  strata_sums <- props[, .(strata_sum = sum(proportion)), by = strata_names]
  props <- merge(props, strata_sums, all = FALSE, by = strata_names)
  props[, scaled_prop := proportion / strata_sum]
  props
}

check_proportions <- function(tabular, prop_name, summands) {
  prop_sums <- props[, lapply(.SD, sum), .SDcols = prop_name, by = summands]
  if (!isTRUE(all.equal(1L, unique(prop_sums[[prop_name]]))))
    stop("not all proportions sum to 1 within ", paste(summands, collapse = ", "))
  else TRUE
}

check_levels <- function(variable, group_means, targets) {
  if (!identical(class(group_means[[variable]]), class(targets[[variable]]))) {
    stop("'", variable, "' inherits from '", class(group_means[[variable]]),
      "' in group_means and '", class(targets[[variable]]), "' in targets")
  }
  else if (!all(unique(group_means[[variable]] %in% targets[[variable]]))) {
    stop("not all values of '", variable, "' in group_means are values of '", variable, "' in targets: ",
      paste(sort(setdiff(group_means[[variable]], targets[[variable]])), collapse = ", "))
  }
  else TRUE
}

# assertthat::assert_that(assertthat::not_empty(group_means))
# assertthat::assert_that(assertthat::not_empty(targets))
# assertthat::assert_that(all_strings(strata_names))
# assertthat::assert_that(all_strings(group_names))
# assertthat::assert_that(assertthat::is.string(prop_name))

# assertthat::assert_that(assertthat::has_name(group_means, variable))
# assertthat::assert_that(assertthat::has_name(targets, variable))

# assertthat::assert_that(assertthat::is.string(prop_name))
# assertthat::assert_that(is.character(summands) && all(nchar(summands) > 0))

# setGeneric("poststratify")
# setMethod("poststratify", method.skeleton("poststratify", c(group_means = "dgirtFit")),
#           function(object, ...) {
#             dots <- list(...)
#             posterior_means <- setDT(as.data.frame(get_posterior_mean(dgirt_out)), keep.rownames = TRUE)
#             prop_name <- dgirt_out@control@prop_name
#
#             if (length(dots$target_data)) {
#               targets <- setDT(copy(dots$target_data))
#             } else if (length(dgirt_out@dgirt_in$target_data)) {
#               targets <- dgirt_out@dgirt_in$target_data
#             } else {
#               stop("Population targets must appear as `target_data` either in the call to `poststratify` or in the `dgirt_in` slot.")
#             }
#             callGeneric(posterior_means, targets, prop_name, ...)
#           })
#
