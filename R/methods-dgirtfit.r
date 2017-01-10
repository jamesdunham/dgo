utils::globalVariables(c("value", "iteration", ".", "rn", "total", "warmup",
                         "Rhat"))

#' @rdname dgirtfit-methods
#' @param x A \code{dgirtfit-class} object
#' @param object A \code{dgirtfit-class} object
#' @param pars Parameter name(s)
#' @param ... Further arguments to \code{\link{stanfit-class}} methods.
#' @export
setMethod("show", "dgirtfit",
          function(object) {
            print.dgirtfit(object)
          })

# Generic documented for dgirtIn
setGeneric("print", signature = "x",
           function(x, ...) standardGeneric("print"))

#' @rdname dgirtfit-methods
setMethod("print", "dgirtfit", function(x, ...) print.dgirtfit(x, ...))

#' \code{print} method for \code{dgirtfit-class} objects
#'
#' @return NULL
#' @rdname dgirtfit-methods
print.dgirtfit <- function(x, ...) {
  ctrl <- x@dgirt_in$control
  sf <- x
  class(sf) <- "stanfit"
  ss <- summary(sf, ..., use_cache = FALSE)
  ss <- ss[["summary"]]
  arg <- x@stan_args[[1]]
  chains <- length(x@stan_args)
  pkg_version <- ifelse(length(x@dgirt_in$pkg_version),
                        paste(x@dgirt_in$pkg_version, collapse = "."),
                        "not available (< 0.2.2)")
  cat("dgirt samples from", chains, "chains of", arg$iter,
      "iterations,", arg$warmup, "warmup, thinned every", arg$thin,
      "\n")

  cat("  Drawn", x@date, "\n")
  cat("  Package version", pkg_version, "\n")
  cat("  Model version", x@model_name, "\n")
  cat(" ", nrow(ss), "parameters; ")
  cat(sum(grepl("^theta_bar", rownames(ss))), "theta_bars ")
  cat("(", concatenate::cc_and(ctrl@time_name, ctrl@geo_name,
        ctrl@group_names), ")", "\n", sep = "")
  cat(" ", x@dgirt_in$T, "periods", min(ctrl@time_filter), "to",
      max(ctrl@time_filter), "\n")

  cat("\nn_eff\n")
  message(paste0(capture.output(summary((ss[, "n_eff"]))),
                                collapse = "\n"))

  cat("\nRhat\n")
  message(paste0(capture.output(summary((ss[, "Rhat"]))),
                                collapse = "\n"))

  cat("\nElapsed time\n")
  message(paste0(capture.output(get_elapsed_time(x)), collapse = "\n"))
}

#' \code{get_elapsed_time}: extract chain run times from \code{dgirtfit}-class
#' objects
#' @rdname dgirtfit-methods
setMethod("get_elapsed_time", "dgirtfit", function(object, ...) {
  # the stanfit method gives a matrix of time in seconds; we'd find a format
  # like 11D 2H 10M more useful
  class(object) = "stanfit"
  elapsed_time = rstan::get_elapsed_time(object)
  times = data.frame(chain = seq_len(nrow(elapsed_time)),
                     elapsed_time, row.names = NULL)
  data.table::setDT(times)[, c("warmup", "sample") := lapply(.SD, function(x)
                              round(lubridate::seconds_to_period(x), 0)),
                                .SDcols = c("warmup", "sample")]
  times[, total := warmup + sample]
  times[]
})

#' \code{summary} method for \code{dgirtfit-class} objects
#'
#' @param verbose Whether to show the full output from the \code{rstan} method.
#'
#' @rdname dgirtfit-methods
#' @export
setMethod("summary", "dgirtfit", function(object, ..., verbose = FALSE) {
  if (isTRUE(verbose)) {
    sf <- object
    class(sf) <- "stanfit"
    summary(sf, ...)
  } else {
    print(object)
  }
})

#' @rdname dgirtfit-methods
#' @export
setMethod("get_posterior_mean", "dgirtfit",
          function(object, pars = "theta_bar", ...) {
            summarize(object, pars, funs = "mean")
          })

#' @rdname dgirtfit-methods
#' @export
setGeneric("summarize", signature = "x",
           function(x, ...) standardGeneric("summarize"))

#' \code{summarize} method for \code{dgirtfit-class} objects
#'
#' @param funs Quoted names of summary functions. `q_025` is accepted as
#'   shorthand for `function(x) quantile(x, .025)`, and similarly `q_975`.
#' @rdname dgirtfit-methods
#' @export
setMethod("summarize", "dgirtfit",
  function(x, pars = "theta_bar",
              funs = c("mean", "sd", "median", "q_025", "q_975")) {
    samples <- as.data.frame(x, pars = pars)
    ctrl <- x@dgirt_in$control
    res <- samples[, do_funs(list(value), funs),
                   by = c(attr(samples, "id_vars"))]
    data.table::setnames(res, grep("^V\\d+", names(res), value = TRUE), funs)
    setattr(res, "id_vars", attr(samples, "id_vars"))
    res
})
q_025 <- function(x) quantile(x, .025)
q_975 <- function(x) quantile(x, .975)
do_funs <- function(value, funs) lapply(funs, function(f) do.call(f, value))

#' \code{as.data.frame} method for \code{dgirtfit-class} objects
#'
#' @param keep.rownames Whether to retain original parameter names with numeric
#'   indexes, as output from RStan.
#'
#' @rdname dgirtfit-methods
#' @export
#'
#' @examples
#' # access posterior samples
#' as.data.frame(toy_dgirtfit, pars = 'theta_bar')
as.data.frame.dgirtfit <- function(x, ..., pars = "theta_bar",
                                   keep.rownames = FALSE) {
  ctrl <- x@dgirt_in$control
  estimates <- as.data.frame.matrix(t(as.matrix(x, pars = pars)))
  estimates <- data.table::setDT(estimates, keep.rownames = TRUE)

  ftab <- flatnames(x)
  ftab <- merge(estimates[, .(rn)], ftab, all = FALSE, by.x = "rn", by.y = "fname")
  all_na <- sapply(ftab, function(x) all(is.na(x)))
  if (any(all_na))
    ftab[, names(all_na)[all_na] := NULL]
  estimates <- merge(estimates, ftab, all.x = TRUE, by = "rn")
  if (!isTRUE(keep.rownames)) {
    estimates[, c("rn") := NULL]
  }
  estimate_names <- grep("^V\\d+", names(estimates), value = TRUE)
  id_vars <- intersect(names(estimates), names(ftab))
  melted <- data.table::melt(estimates, id.vars = id_vars, variable.name =
                             "iteration", variable.factor = FALSE)
  melted[, c("iteration") := type.convert(sub("V", "", get("iteration"),
                                              fixed = TRUE))]
  setkeyv(melted, id_vars)
  data.table::setattr(melted, "id_vars", id_vars)
  melted
}

#' @rdname dgirtfit-methods
setGeneric("rhats", signature = "x", function(x, ...)
           standardGeneric("rhats"))

#' \code{rhats}: extract split R-hats from \code{dgirtfit}-class objects
#'
#' @return A table giving split R-hats for model parameters
#' @export
#' @rdname dgirtfit-methods
#' @examples
#' rhats(toy_dgirtfit)
setMethod("rhats", signature(x = "dgirtfit"),
          function(x, pars = "theta_bar") {
  assert(all_strings(pars))
  fnames = flatnames(x)
  rhats = summary(x, par = pars, verbose = TRUE)$summary[, "Rhat", drop = FALSE]
  rhats = data.table::setDT(as.data.frame(rhats), keep.rownames = TRUE)
  rhats = rhats[fnames, on = c("rn" = "fname")][!is.na(Rhat)]
  drop_cols = names(rhats)[vapply(rhats, function(x) all(is.na(x)), logical(1))]
  if (length(drop_cols)) {
    rhats[, c(drop_cols) := NULL]
  }
  rhats[, c("rn") := NULL]
  data.table::setcolorder(rhats, c(setdiff(names(rhats), "Rhat"), "Rhat"))
  rhats[]
})
