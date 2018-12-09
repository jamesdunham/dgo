#' A class for fitted dynamic group multinomial IRT models
#'
#' \code{\link{mgirt}} returns a fitted model object of class \code{mgirt_fit},
#' which inherits from \code{\link[rstan]{stanfit-class}} in the
#' \code{\link{rstan}} package.
#'
#' @slot mgirt_in \code{\link{mgirtin-class}} data used to fit the model.
#' @slot call The function call that returned the \code{mgirt_fit} object.
#'
#' @aliases mgirt_fit mgirt_fit-class
#' @name mgirt_fit-class
mgirt_fit <- setClass("mgirt_fit", contains = "stanfit",
  slots = list(mgirt_in = "ANY", call = "language"))
