#' State-level demographic marginals
#'
#' A table giving the proportion of the American population 1960-2010 falling
#' into strata defined by five demographic variables.
#'
#' \itemize{
#'  \item `year`, an integer ranging 1960-2010.
#'  \item `state`, a factor variable with 51 levels.
#'  \item `race`, a factor variable with 3 levels, `black`, `other`, and
#'  `white.`
#'  \item `female`, a factor variable with 2 levels, `female` and `male`.
#'  \item `education`, an integer ranging 1-5.
#'  \item `age`, an integer ranging 1-4.
#'  \item `proportion`, a numeric that sums to 1 within each `year`.
#' }
#'
#' @docType data
#' @name state_demographics
#' @usage state_demographics
#' @format A `data.frame` with 312,120 rows and 7 columns.
#' @examples
#' state_demographics
NULL
