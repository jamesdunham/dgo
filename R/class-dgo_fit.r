#' Class \code{dgo_fit}: a class for fitted models
#'
#' \code{dgo_fit} is a superclass for \code{\link{dgirt_fit}} and
#' \code{\link{dgmrp_fit}} that inherits from the
#' \code{\link[rstan]{stanfit-class}} in the \code{\link{rstan}} package.
#'
#' @slot dgirt_in \code{\link{dgirtin-class}} data used to fit the model.
#'
#' @aliases dgo_fit dgo_fit-class
#' @seealso \code{\link{dgmrp_fit}} \code{\link{dgo_fit}} 
#' @name dgo_fit-class
#' @export
dgo_fit <- setClass("dgo_fit", contains = "stanfit",
  slots = list(dgirt_in = "ANY", call = "language"))
