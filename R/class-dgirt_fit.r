#' Class \code{dgirt_fit}: a class for fitted models
#'
#' \code{\link{dgirt}} returns a fitted model object of class \code{dgirt_fit},
#' which inherits from \code{\link{dgo_fit}}.
#'
#' dgo 0.2.8 deprecated the \code{dgirtfit} class and replaced it with the
#' \code{\link{dgirt_fit}} class. 
#'
#' @slot dgirt_in \code{\link{dgirtin-class}} data used to fit the model.
#'
#' @aliases dgirt_fit dgirt_fit-class
#' @seealso \code{\link{dgmrp_fit}} \code{\link{dgo_fit}} 
#' @name dgirt_fit-class
#' @include class-dgo_fit.r
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
dgirt_fit <- setClass("dgirt_fit", contains = c("dgo_fit"))

#' Class \code{dgirtfit}: a class for fitted models
#'
#' dgo 0.2.8 deprecated the \code{dgirtfit} class and replaced it with the
#' \code{\link{dgirt_fit}} class. 
#'
#' @name dgirtfit-class
#' @aliases dgirtfit dgirtfit-class
dgirtfit <- setClass("dgirtfit", contains = c("dgirt_fit"))
