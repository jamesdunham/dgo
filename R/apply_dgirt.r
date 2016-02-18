#' Summarize dgirt samples
#'
#' `apply_dgirt` applies a scalar function over dgirt sampler iterations for each parameter that appears in the `stanfit`
#' object returned by `dgirt`.
#' 
#' @param dgirt_output Return value of `dgirt`, a `stanfit` object.
#' @param dgirt_input Return value of `wrangle`.
#' @param fun A single scalar function like `mean`.
#' @return A list of tables summarizing the posterior distribution of each model parameter.
#' @export
apply_dgirt <- function(dgirt_output, dgirt_input, fun = mean.default) {
  assertthat::assert_that(inherits(dgirt_output, "stanfit"))
  assertthat::assert_that(assertthat::not_empty(dgirt_input$vars))

  dgirt_extract <- rstan::extract(dgirt_output)
  vars <- dgirt_input$vars

  dgirt_summary = lapply(dgirt_extract, function(element) {
    assertthat::assert_that(assertthat::not_empty(element))
    if (!is.null(dim(element)) && length(dim(element)) > 1) {
      over_dims = seq.int(2, length(dim(element)))
      out = apply(element, over_dims, fun)
    } else {
      out = fun(element)
    }
    if (length(out) > 1) {
      out = reshape2::melt(out)
    }
    return(out)
  })

  dgirt_summary = name_output_dims(dgirt_summary, vars)

  return(dgirt_summary)
}
