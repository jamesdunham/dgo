#' Extract and name parameters
#'
#' `extract_dgirt` is a wrapper for rstan::extract that attaches
#' names to parameters according to the values of the data passed
#' to `dgirt`.
#' @param stan_output Return value of `dgirt`.
#' @param stan_data Return value of `wrangle`.
#' @param fun A function that can be applied to a scalar vector without arguments (e.g. `mean`).
#' @return Return value of `rstan::extract` with names attached to its elements.
#' @export
extract_dgirt <- function(stan_output, stan_data, fun = mean.default) {
  assertthat::assert_that(inherits(stan_output, "stanfit"))
  assertthat::assert_that(assertthat::not_empty(stan_data$vars))

  dgirt_extract <- rstan::extract(stan_output)
  vars <- stan_data$vars

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
