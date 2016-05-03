#' \code{show} method for \code{dgirtfit-class} objects
#' @param object An object of class \code{dgirtIn} as returned by \code{shape}.
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
#' @export
#' @examples
#' get_posterior_mean(toy_dgirtfit)
setMethod("get_posterior_mean", "dgirtfit",
          function(object = dgirtfit, ...) {
            posterior_means <- callNextMethod(object, ...)
            rownames(posterior_means) <- flatnames(object,
              rownames(posterior_means))
            posterior_means
          })
