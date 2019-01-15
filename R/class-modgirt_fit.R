#' A class for fitted MODGIRT models
#'
#' \code{\link{modgirt}} returns a fitted model object of class \code{modgirt_fit},
#' which inherits from \code{\link[rstan]{stanfit-class}} in the
#' \code{\link{rstan}} package.
#'
#' @slot call The function call that returned the \code{modgirt_fit} object.
#' @slot items Names of item response variables.
#' @slot time Name of a time variable.
#' @slot geo Name of a geographic variable.
#' @slot demo Names of further grouping variables, usually demographic.
#' @aliases modgirt_fit modgirt_fit-class
#' @name modgirt_fit-class
modgirt_fit <- setClass("modgirt_fit",
  slots = list(
    items = "character",
    time = "character",
    geo = "character",
    demo = "character",
    stan_data_dimnames = "list",
    call = "language",
    stanfit = "stanfit"
))

# Show method
#' @rdname modgirt_fit-methods
#' @param x A \code{modgirt_fit-class} object
#' @param object A \code{modgirt_fit-class} object
#' @param pars Parameter name(s)
#' @param ... Further arguments to \code{\link{stanfit-class}} methods.
#' @export
setMethod("show", "modgirt_fit",
  function(object) {
    print.modgirt_fit(object)
})

# S4 print method
#' @rdname modgirt_fit-methods
#' @include methods-dgirtfit.r
setMethod("print", "modgirt_fit", function(x) print.modgirt_fit(x))

# S4 summary method
setMethod("summary", "modgirt_fit", function(object, ...)
  print.modgirt_fit(object))

# S3 print method
#' \code{print} method for \code{modgirt_fit-class} objects
#'
#' @return NULL
#' @rdname modgirt_fit-methods
print.modgirt_fit <- function(x) {
  rstan_args <- x@stanfit@stan_args[[1]]
  cat("modgirt samples")
  cat(" from", length(x@stanfit@stan_args), "chain(s)", fill = TRUE)
  cat("  Drawn", x@stanfit@date, fill = TRUE)
  cat("  ", rstan_args$iter, " iterations (", rstan_args$warmup, " warmup)", sep = "")
  if (length(rstan_args$thin) && rstan_args$thin > 1) {
    cat(", thinned by", rstan_args$thin)
  }
  cat(fill = TRUE)
  cat(" ", length(x@stanfit@sim$fnames_oi), "parameters", fill = TRUE)
  cat(" ", sum(grepl("^bar_theta", x@stanfit@sim$fnames_oi)), "group parameters ")
  cat("(", paste(x@time, x@geo, paste(x@demo, collapse = " x "), sep = " x "),
    ")", sep = "", fill = TRUE)
}

#' Chain run times
#' @rdname modgirt_fit-methods
setMethod("get_elapsed_time", "modgirt_fit", function(object, ...) {
  # the stanfit method gives a matrix of time in seconds
  elapsed_time = rstan::get_elapsed_time(object@stanfit)
  times = data.frame(chain = seq_len(nrow(elapsed_time)),
                     elapsed_time, row.names = NULL)
  # reformat with lubridate
  data.table::setDT(times)[, c("warmup", "sample") := lapply(.SD, function(x)
    round(lubridate::seconds_to_period(x), 0)), .SDcols = c("warmup", "sample")]
  times[, total := warmup + sample]
  times[]
})

#' Split R-hats
#'
#' @return A table giving split R-hats for model parameters
#' @include methods-dgirtfit.r
#' @export
setMethod("rhats", signature(x = "modgirt_fit"), function(x, pars = "bar_theta") {
  # extract r-hats from stanfit's summary method output
  rhat = summary(x@stanfit, par = pars, verbose = TRUE)$summary[, "Rhat", drop = FALSE]
  # the result is a matrix whose rownames give parameters
  rhat = setDT(as.data.frame(rhat), keep.rownames = TRUE)
  setnames(rhat, 'rn', 'fname')
  # map flatname indices to labels, as in the as.data.frame method
  param_labels <- label_params(x, unique(rhat$fname))
  rhat <- merge(rhat, param_labels, all.x = TRUE, by = "fname")
  rhat[, fname := NULL]
  data.table::setcolorder(rhat, c(setdiff(names(rhat), "Rhat"), "Rhat"))
  rhat[]
})

as.data.frame.modgirt_fit <- function(x, pars = "bar_theta", keep_fname = FALSE,
  ...) {
  # columns give parameter flatnames (e.g., "bar_theta[1,6,1]")
  estimates <- data.table(as.matrix(x@stanfit, pars = pars))
  # rows represent iterations
  estimates[, iteration := .I]
  estimates = melt(estimates, id.vars = 'iteration', variable.name = 'fname',
    variable.factor = FALSE)
  # map flatname indices to labels
  param_labels <- label_params(x, unique(estimates$fname))
  estimates <- merge(estimates, param_labels, all.x = TRUE, by = "fname")
  if (!isTRUE(keep_fname)) {
    estimates[, fname := NULL]
  }
  last_cols = c('iteration', 'value')
  data.table::setcolorder(estimates, c(setdiff(names(estimates), last_cols), last_cols))
  estimates[]
}

label_params <- function(x, fnames = NULL) {
  if (!length(fnames)) {
    fnames <- x@stanfit@sim$fnames_oi
  }

  fname_len <- length(fnames)
  ftab <- data.table::setDT(list(fname = fnames))

  # for the given fnames, there are no more than this many comma-separated
  # splits; minimum 1, if fnames contains none of [],
  splits = max(lengths(strsplit(ftab$fname, '\\[|\\]|,')))
  # which includes the param name, so use this many index columns less 1, with
  # names i, ...
  indexes = letters[seq(9, length.out = splits - 1)]
  ftab[, c('param', indexes) := tstrsplit(fname, '\\[|\\]|,', keep = seq.int(splits))]
  if (length(indexes)) {
    ftab[, c(indexes) := lapply(.SD, as.integer), .SDcols = indexes]
  }

  for (parname in unique(ftab$param)) {
    # look up the names of parameter indexes
    for (idx_name in modgirt_indices[[parname]]) {
      if (length(idx_name)) {
        # move integer index values from columns i/j/k to the appropriate named
        # column like 'item' or 'dimension'
        pos <- indexes[which(modgirt_indices[[parname]] == idx_name)]
        ftab[param == parname, c(idx_name) := get(pos)]
      }
    }
  }

  if ("group_index" %in% names(ftab)) {
    group_names = setDT(
      setNames(tstrsplit(x@stan_data_dimnames[[2]], '__', fixed = TRUE),
        c(x@geo, x@demo)))
    group_names[, group_index := .I]
    ftab <- merge(ftab, group_names, by = "group_index", all.x = TRUE)
  }

  if ("time_index" %in% names(ftab)) {
    time_names = setDT(setNames(list(x@stan_data_dimnames[[1]]), x@time))
    time_names[, time_index := .I]
    ftab <- merge(ftab, time_names, by = "time_index", all.x = TRUE)
  }

  if ("item_index" %in% names(ftab)) {
    item_names = data.table(item = x@stan_data_dimnames[[3]])
    item_names[, item_index := .I]
    ftab <- merge(ftab, item_names, by = "item_index", all.x = TRUE)
  }

  drop_cols <- intersect(names(ftab), c("item_index", "time_index",
      "group_index", indexes))
  if (length(drop_cols)) {
    ftab[, c(drop_cols) := NULL]
  }

  stopifnot(identical(nrow(ftab), fname_len))
  stopifnot(!anyDuplicated(ftab, by = 'fname'))
  ftab
}

modgirt_indices = list(
  "raw_bar_theta_N01" = c('time_index', 'group_index', 'dimension'),
  "raw_alpha" = c('time_index', 'item_index'),
  "beta_free" = c('dimension', 'item_index'),
  "beta_neg" = c('dimension', 'item_index'),
  "beta_pos" = c('dimension', 'item_index'),
  "sd_theta_N01" = 'dimension',
  "sd_theta_IG" = 'dimension',
  "sd_bar_theta_evolve" = 'dimension',
  "sd_raw_bar_theta_evolve" =  'dimension',
  "sd_raw_bar_theta_evolve_N01" =  'dimension',
  "sd_raw_bar_theta_evolve_IG" =  'dimension',
  "raw_bar_theta" = c('time_index', 'group_index', 'dimension'),
  "bar_theta" = c('time_index', 'group_index', 'dimension'),
  "beta" = c('item_index', 'dimension'),
  "alpha" = c('time_index', 'item_index'),
  "sd_theta" = 'dimension',
  "Sigma_theta" = 'dimension',
  'mean_raw_bar_theta' = 'dimension'
)


