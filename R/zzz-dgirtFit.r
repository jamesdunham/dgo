#' `show`: extract samples from fitted DGIRT model
#' @rdname dgirfit-class
setMethod("show", "dgirtFit",
          function(object = dgirtFit) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object)
          })

#' `show`: extract samples from fitted DGIRT model
#' @rdname dgirfit-class
setMethod("summary", "dgirtFit",
          function(object = dgirtFit, ...) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object, ...)
          })

#' @rdname dgirfit-class
setMethod("extract", "dgirtFit",
          function(object = dgirtFit, ...) {
            dots <- list(...)
            extracted <- callNextMethod(object, ...)
            if (!length(dots$permuted) || dots$permuted)
              # permuted is TRUE so extract returns a list of arrays
              extracted <- arraynames(extracted, object)
            else
              # permuted = FALSE so extract returns an array with dimensions iterations x chains x parameters
              dimnames(extracted)[[3]] <- flatnames(object, dimnames(extracted)[[3]])
            extracted
          }) 

#' @rdname dgirfit-class
setMethod("get_posterior_mean", "dgirtFit",
          function(object = dgirtFit, ...) {
            posterior_means <- callNextMethod(object, ...)
            rownames(posterior_means) <- flatnames(object, rownames(posterior_means))
            posterior_means
          })
