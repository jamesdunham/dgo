#' Class \code{dgirtfit}: a class for fitted DGIRT modelsf
#'
#' Fitting a dgirt model results in a \code{dgirtfit} object that inherits from
#' \code{\link{rstan}}'s \code{\link[rstan]{stanfit-class}}. \code{rstan}
#' methods will be dispatched (only) if a \code{dgirtfit} method does
#' not exist.
#'
#' @slot dgirt_in \code{\link{dgirtin-class}} data used to fit the model.
#'
#' @aliases dgirtfit dgirtfit-class
#' @seealso \code{\link{stanfit-class}} \code{\link{dgirtin-class}}
#' @rdname dgirtfit-class
#' @name dgirtfit-class
#' @examples
#' data(toy_dgirtfit)
#' # summarize the fitted results
#' summary(toy_dgirtfit, pars = 'xi')
#'
#' # get posterior means with a convenience function
#' get_posterior_mean(toy_dgirtfit, pars = 'theta_bar')
#'
#' # generally apply functions to posterior samples after warmup; n.b.
#' # `as.array` is iterations x chains x parameters so `MARGIN = 3` applies
#' # `FUN` over iterations and chains
#' apply(as.array(toy_dgirtfit, pars = 'xi'), 3, mean)
#'
#' # access the posterior samples
#' as.array(toy_dgirtfit, pars = 'theta_bar')
#' as.data.frame(toy_dgirtfit, pars = 'theta_bar')
#' extract(toy_dgirtfit, pars = 'theta_bar')
dgirtfit <- setClass("dgirtfit",
  contains = "stanfit",
  slots = list(dgirt_in = "ANY",
    call = "language"))