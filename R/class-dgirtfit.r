#' Class \code{dgirtfit}: fitted DGIRT model
#'
#' All \code{\link{rstan}} methods for superclass
#' \code{\link[rstan]{stanfit-class}} are available. Descriptive labels for
#' parameters on time periods, local geographic areas, and grouping variables
#' will be added to most output.
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
