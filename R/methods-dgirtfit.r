#' \code{show} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @export
#' @examples
#' toy_dgirtfit
setMethod("show", "dgirtfit",
          function(object = dgirtfit) {
            object@sim$fnames_oi <- flatnames(object)
            print(object)
          })

#' \code{summary} method for \code{dgirtfit-class} objects
#' @rdname dgirtfit-class
#' @param ... Further arguments to \code{\link{stanfit-class}} methods.
#' @export
#' @examples
#' summary(toy_dgirtfit)
setMethod("summary", "dgirtfit",
          function(object = dgirtfit, ...) {
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
          function(object = dgirtfit, ...) {
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
          function(object = dgirtfit, pars = 'theta_bar', name = TRUE, ...) {
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
#' @include poststratify.r 
#' @rdname poststratify
#' @export
setMethod("poststratify", c("data.frame"),
  function(x = data.frame, target_data, strata_names, group_names, prop_name =
           "proportion", aggregate = FALSE, pars = "theta_bar")  {
    post_generic(x, target_data = target_data, strata_names = strata_names,
                 group_names = group_names, prop_name = prop_name,
                 aggregate = aggregate, pars = pars)
  })

#' \code{poststratify} method for \code{dgirtfit}-class objects
#' @include poststratify.r 
#' @param x A \code{dgirtfit}-class object.
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
#' @param pars The names of model parameters of interest. Others will be
#' excluded.
#'
#' @return A table giving poststratified estimates for each stratum.
#' @export
#' @rdname poststratify 
#' @export
setMethod("poststratify", c("dgirtfit"),
  function(x = dgirtfit, target_data, strata_names, prop_name = "proportion",
           aggregate = FALSE, pars = "theta_bar") {
    ctrl <- x@dgirt_in$control
    estimates <- t(as.data.frame(x, par = pars))
    estimates <- expand_rownames(estimates, geo_name = ctrl@geo_name,
                                 group_names = ctrl@group_names, time_name =
                                   ctrl@time_name)
    res <- post_generic(x = estimates, target_data = target_data,
                        group_names = ctrl@group_names,
                        strata_names = strata_names,
                        prop_name = prop_name,
                        aggregate = aggregate)
    melted <- data.table::melt(res, id.vars = strata_names, variable.name =
                             "iteration", variable.factor = FALSE)
    melted[, c("iteration") := type.convert(sub("V", "", get("iteration"),
                                              fixed = TRUE)), with = FALSE]
    return(melted[])
  })
