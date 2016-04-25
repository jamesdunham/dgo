#' @rdname dgirfit-class
setMethod("show", "dgirtFit",
          function(object = dgirtFit) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object)
          })

#' @rdname dgirfit-class
setMethod("summary", "dgirtFit",
          function(object = dgirtFit, ...) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object, ...)
          })

#' @rdname dgirfit-class
setMethod("extract", "dgirtFit",
          function(object = dgirtFit, ...) {
            extracted <- callNextMethod(object, ...)
            if (is.list(extracted)) {
              extracted <- arraynames(extracted, object)
            } else if (is.array(extracted)) {
              dimnames(extracted)[[3]] <- flatnames(object,
                dimnames(extracted)[[3]])
            }
            extracted
          }) 

#' @rdname dgirfit-class
setMethod("get_posterior_mean", "dgirtFit",
          function(object = dgirtFit, ...) {
            posterior_means <- callNextMethod(object, ...)
            rownames(posterior_means) <- flatnames(object,
              rownames(posterior_means))
            posterior_means
          })
