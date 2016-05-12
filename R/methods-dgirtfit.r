#' \code{show} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @export
#' @examples
#' toy_dgirtfit
setMethod("show", "dgirtfit",
          function(object) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object)
          })

#' \code{summary} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @param ... Further arguments to \code{\link{stanfit-class}} methods.
#' @export
#' @examples
#' summary(toy_dgirtfit)
setMethod("summary", "dgirtfit",
          function(object, ...) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object, ...)
          })

#' \code{extract} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @param object A \code{dgirtfit}-class object.
#' @export
#' @examples
#' extract(toy_dgirtfit)
setMethod("extract", "dgirtfit",
          function(object, ...) {
            extracted <- callNextMethod(object, ...)
            if (is.list(extracted)) {
              extracted <- arraynames(extracted, object)
            } else if (is.array(extracted)) {
              dimnames(extracted)[[3]] <- flatnames(object,
                dimnames(extracted)[[3]])
            }
            extracted
          })

#' \code{get_posterior_mean} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @param pars Selected parameter names.
#' @param name Whether to move descriptive rownames into columns.
#' @export
#' @examples
#' get_posterior_mean(toy_dgirtfit)
setMethod("get_posterior_mean", "dgirtfit",
          function(object, pars = 'theta_bar', name = TRUE, ...) {
            posterior_means <- callNextMethod(object, pars = pars, ...)
            rownames(posterior_means) <- flatnames(object,
              rownames(posterior_means))
            if (isTRUE(name)) {
              ctrl <- object@dgirt_in$control
              posterior_means <- expand_rownames(posterior_means,
                geo_name = ctrl@geo_name, group_names = ctrl@group_names,
                time_name = ctrl@time_name)
            }
            posterior_means
          })

#' \code{poststratify}: reweight and aggregate estimates
#' @rdname poststratify
#' @param ... Additional arguments to methods.
setGeneric("poststratify", signature = "x",
           function(x, prop_name = "proportion", ...) {
            standardGeneric("poststratify")
           })

#' \code{poststratify} method for \code{data.frame}s
#'
#' Identifiers in the table of estimates to be poststratified should be given as
#' \code{strata_names} or \code{aggregated_names}. There will be a row in the
#' result for each interaction of the variables in \code{strata_names}
#' containing the values of \code{estimate_names} poststratified over the
#' variables in \code{aggregated_names}.
#'
#' @param x A \code{data.frame}.
#' @param target_data A table giving the proportions contributed to strata by
#' the interaction of \code{strata_names} and \code{aggregated_names}.
#' @param strata_names Names of variables whose interaction defines
#' population strata.
#' @param aggregated_names Names of variables to be aggregated over in
#' poststratification. 
#' @param estimate_names Names of columns to be poststratified.
#' @param prop_name Name of the column in \code{target_data} that gives
#' strata proportions.
#' @param keep Whether to keep the original estimates and return them alongside
#' the poststratified estimates. 
#' @return A table of poststratified estimates.
#' @include poststratify.r 
#' @rdname poststratify
#' @export
setMethod("poststratify", c("data.frame"),
  function(x, target_data, strata_names, aggregated_names, estimate_names,
           prop_name = "proportion", keep = FALSE) {
    post_generic(x, target_data, strata_names, aggregated_names, estimate_names,
                 prop_name, keep = FALSE)
  })

#' \code{poststratify} method for \code{dgirtfit}-class objects
#' @include poststratify.r 
#' @param pars Selected parameter names.
#' @export
#' @rdname poststratify 
#' @export
setMethod("poststratify", c("dgirtfit"),
  function(x, target_data, strata_names, aggregated_names, estimate_names,
           prop_name = "proportion", keep = FALSE, pars = "theta_bar") {
    ctrl <- x@dgirt_in$control
    estimates <- t(as.data.frame(x, par = pars))
    estimates <- expand_rownames(estimates, geo_name = ctrl@geo_name,
                                 group_names = ctrl@group_names,
                                 time_name = ctrl@time_name)
    estimate_names <- grep("^V\\d+", names(estimates), value = TRUE)
    res <- post_generic(x = estimates, target_data = target_data,
                        strata_names = strata_names,
                        aggregated_names = aggregated_names,
                        estimate_names = estimate_names,
                        prop_name = prop_name)
    melted <- data.table::melt(res, id.vars = strata_names, variable.name =
                             "iteration", variable.factor = FALSE)
    melted[, c("iteration") := type.convert(sub("V", "", get("iteration"),
                                              fixed = TRUE)), with = FALSE]
    return(melted[])
  })
