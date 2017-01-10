#' Class \code{dgmrp_fit}: a class for fitted models
#'
#' \code{\link{dgmrp}} returns a fitted model object of class \code{dgmrp_fit},
#' which inherits from \code{\link{dgo_fit}}.
#'
#' @slot dgirt_in \code{\link{dgirtin-class}} data used to fit the model.
#'
#' @aliases dgmrp_fit dgmrp_fit-class
#' @seealso \code{\link{dgirt_fit}} \code{\link{dgo_fit}} 
#' @include class-dgo_fit
#' @name dgmrp_fit-class
dgmrp_fit <- setClass("dgmrp_fit", contains = "dgo_fit")
