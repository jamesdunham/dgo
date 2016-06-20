utils::globalVariables(c("value", "iteration", ".", "rn"))

setMethod("show", "dgirtfit",
          function(object) {
            print.dgirtfit(object)
          })

setGeneric("print", signature = "x",
           function(x, ...) standardGeneric("print"))

setMethod("print", "dgirtfit", function(x, ...) print.dgirtfit(x, ...))

print.dgirtfit <- function(x, ...) {
  ctrl <- x@dgirt_in$control
  sf <- x
  class(sf) <- "stanfit"
  ss <- summary(sf, ..., use_cache = FALSE)
  ss <- ss[["summary"]]
  arg <- x@stan_args[[1]]
  chains <- length(x@stan_args)
  pkg_version <- ifelse(length(x@dgirt_in$package_version),
                        x@dgirt_in$package_version,
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
  print(summary((ss[, "n_eff"])))

  cat("\nRhat\n")
  print(summary(ss[, "Rhat"]))
}

setMethod("summary", "dgirtfit", function(object, ..., verbose = FALSE) {
  if (isTRUE(verbose)) {
    sf <- object
    class(sf) <- "stanfit"
    summary(sf, ...)
  } else {
    object
  }
})

setMethod("get_posterior_mean", "dgirtfit",
          function(object, pars = "theta_bar", ...) {
            summarize(object, pars, funs = "mean")
          })

setGeneric("summarize", signature = "x",
           function(x, ...) standardGeneric("summarize"))

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

as.data.frame.dgirtfit <- function(x, ..., pars = "theta_bar",
                                   keep.rownames = FALSE) {
  ctrl <- x@dgirt_in$control
  estimates <- as.data.frame.matrix(t(as.matrix(x, pars = pars)))
  estimates <- data.table::setDT(estimates, keep.rownames = TRUE)

  ftab <- flatnames(x)
  ftab <- merge(estimates[, .(rn)], ftab, all = FALSE, by.x = "rn", by.y = "fname")
  all_na <- sapply(ftab, function(x) all(is.na(x)))
  if (any(all_na))
    ftab[, names(all_na)[all_na] := NULL, with = FALSE]
  estimates <- merge(estimates, ftab, all.x = TRUE, by = "rn")
  if (!isTRUE(keep.rownames))
    estimates[, c("rn") := NULL]

  estimate_names <- grep("^V\\d+", names(estimates), value = TRUE)
  id_vars <- intersect(names(estimates), names(ftab))
  melted <- data.table::melt(estimates, id.vars = id_vars, variable.name =
                             "iteration", variable.factor = FALSE)
  melted[, c("iteration") := type.convert(sub("V", "", get("iteration"),
                                              fixed = TRUE)), with = FALSE]
  setkeyv(melted, id_vars)
  data.table::setattr(melted, "id_vars", id_vars)
  melted
}
