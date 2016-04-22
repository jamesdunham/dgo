#' A Minimal Example of the `dgirtfit` Class.
#'
#' `\link{dgirt}` returns a `dgirtfit`-class object that extends the `stanfit`
#' class from `rstan`.  `toy_dgirtfit` is a minimal `dgirtfit` object, mostly
#' for use in development.
#'
#' @docType data
#' @name toy_dgirtfit
#' @usage toy_dgirtfit
#' @format A `dgirtfit` object.
#' @examples
#' # created as follows
#' data(toy_dgirt_in)
#' toy_dgirtfit <- dgirt(dgirt_in, iter = 10, chains = 2)
#' toy_dgirtfit
NULL
