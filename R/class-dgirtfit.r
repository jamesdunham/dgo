#' Fitted DGIRT Model.
#'
#' All `rstan` methods for superclass `\link[rstan]{stanfit}` are available.
#' Descriptive labels for parameters on time periods, local geographic areas,
#' and grouping variables will be added to most output.
#' @seealso `\link[rstan]{stanfit-class}`
#' @slot dgirt_in Data used to fit the model.
#' @aliases dgirtfit dgirtfit-class dgirtFit-class
#' @examples
#' data(toy_dgirtfit)
#' # summarize the fitted results
#' summary(toy_dgirtfit, pars = 'xi')
#' 
#' # get posterior means with a convenience function
#' get_posterior_mean(toy_dgirtfit, pars = 'xi')
#'
#' # generally apply functions to posterior samples after warmup; n.b.
#' # `as.array` is iterations x chains x parameters so `MARGIN = 3` applies
#' # `FUN` over iterations and chains
#' apply(as.array(toy_dgirtfit, pars = 'xi'), 3, mean)
#'
#' # access the posterior samples
#' as.array(toy_dgirtfit, pars = 'xi')  # samples after warmup
#' as.data.frame(toy_dgirtfit, pars = 'xi')  # all samples
#' extract(toy_dgirtfit, pars = 'theta_bar')  # all samples
#' @rdname dgirfit-class
dgirtFit <- setClass("dgirtFit",
                     contains = "stanfit",
                     slots = list(dgirt_in = "ANY",
                                  call = "language"))
