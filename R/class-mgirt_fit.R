#' A class for fitted MODGIRT models
#'
#' \code{\link{modgirt}} returns a fitted model object of class \code{modgirt_fit},
#' which inherits from \code{\link[rstan]{stanfit-class}} in the
#' \code{\link{rstan}} package.
#'
#' @slot modgirt_in \code{\link{modgirt_in-class}} data used to fit the model.
#' @slot call The function call that returned the \code{modgirt_fit} object.

#' @aliases modgirt_fit modgirt_fit-class
#' @name modgirt_fit-class
modgirt_fit <- setClass("modgirt_fit", contains = "stanfit",
  slots = list(modgirt_in = "ANY", call = "language"))

# Show method
#' @rdname modgirt_fit-methods
#' @param x A \code{modgirt_fit-class} object
#' @param object A \code{modgirt_fit-class} object
#' @param pars Parameter name(s)
#' @param ... Further arguments to \code{\link{stanfit-class}} methods.
#' @export
setMethod("show", "modgirt_fit", function(object) {
            print.modgirt_fit(object)
})

# S4 print method
#' @rdname modgirt_fit-methods
setMethod("print", "modgirt_fit", function(x, ...) print.modgirt_fit(x, ...))

# S3 print method
#' \code{print} method for \code{modgirt_fit-class} objects
#'
#' @return NULL
#' @rdname modgirt_fit-methods
print.modgirt_fit <- function(x, ...) {
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
  cat("(", paste(ctrl@time_name, ctrl@geo_name,
        ctrl@group_names, collapse = ", "), ")", "\n", sep = "")
  cat(" ", x@dgirt_in$T, "periods", min(ctrl@time_filter), "to",
      max(ctrl@time_filter), "\n")

  cat("\nn_eff\n")
  message(paste0(capture.output(summary((ss[, "n_eff"]))), collapse = "\n"))

  cat("\nRhat\n")
  message(paste0(capture.output(summary((ss[, "Rhat"]))), collapse = "\n"))

  cat("\nElapsed time\n")
  message(paste0(capture.output(get_elapsed_time(x)), collapse = "\n"))
}

