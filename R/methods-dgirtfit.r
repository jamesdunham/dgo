utils::globalVariables(c("value", "iteration"))

# #' \code{show} method for \code{dgirtfit-class} objects
# #' @rdname dgirtfit-class
# #' @export
# #' @examples
# #' toy_dgirtfit
# setMethod("show", "dgirtfit",
#           function(object) {
#             # object@sim$fnames_oi <- flatnames(object)
#             callNextMethod(object)
#           })
#
# #' \code{summary} method for \code{dgirtfit-class} objects
# #' @rdname dgirtfit-class
# #' @param ... Further arguments to \code{\link{stanfit-class}} methods.
# #' @export
# #' @examples
# #' summary(toy_dgirtfit)
# setMethod("summary", "dgirtfit",
#           function(object, ...) {
#             # object@sim$fnames_oi <- flatnames(object)
#             callNextMethod(object, ...)
#           })
#
# #' \code{extract} method for \code{dgirtfit-class} objects
# #' @rdname dgirtfit-class
# #' @param object A \code{dgirtfit}-class object.
# #' @param x A \code{dgirtfit}-class object.
# #' @export
# #' @examples
# #' extract(toy_dgirtfit)
# setMethod("extract", "dgirtfit",
#           function(object, ...) {
#             extracted <- callNextMethod(object, ...)
#             # if (is.list(extracted)) {
#             #   extracted <- arraynames(extracted, object)
#             # } else if (is.array(extracted)) {
#             #   dimnames(extracted)[[3]] <- flatnames(object,
#             #     dimnames(extracted)[[3]])
#             # }
#             extracted
#           })

#' \code{get_posterior_mean} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @param pars Selected parameter names.
#' @export
#' @examples
#' get_posterior_mean(toy_dgirtfit)
setMethod("get_posterior_mean", "dgirtfit",
          function(object, pars = "theta_bar", ...) {
            samples <- as.data.frame(object, pars = pars)
            ctrl <- object@dgirt_in$control
            samples[, list(mean = mean(value)),
                    by = c(ctrl@time_name, ctrl@geo_name, ctrl@group_names)]
          })

#' \code{as.data.frame} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @param discard Whether to discard samples from warmup iterations.
#' @export
#' @examples
#' as.data.frame(toy_dgirtfit)
as.data.frame.dgirtfit <- function(x, ..., pars = "theta_bar", discard = TRUE) {
  if (length(pars) > 1L)
    stop("\"pars\" should be a single parameter name")
  ctrl <- x@dgirt_in$control
  estimates <- as.data.frame.matrix(t(as.matrix(x, pars = pars)))
  estimates <- data.table::setDT(estimates, keep.rownames = TRUE)

  ftab <- flatnames(x)
  ftab <- merge(estimates[, .(rn)], ftab, all = FALSE, by.x = "rn", by.y = "fname")
  all_na <- sapply(ftab, function(x) all(is.na(x)))
  if (any(all_na))
    ftab[, names(all_na)[all_na] := NULL, with = FALSE]
  estimates <- merge(estimates, ftab, all.x = TRUE, by = "rn")
  estimates[, c("rn") := NULL]

  estimate_names <- grep("^V\\d+", names(estimates), value = TRUE)
  id_vars <- intersect(names(estimates), names(ftab))
  melted <- data.table::melt(estimates, id.vars = id_vars, variable.name =
                             "iteration", variable.factor = FALSE)
  melted[, c("iteration") := type.convert(sub("V", "", get("iteration"),
                                              fixed = TRUE)), with = FALSE]
  if (isTRUE(discard)) {
    warmup <- x@stan_args[[1]]$warmup
    melted <- melted[iteration > warmup]
  }
  melted
}
