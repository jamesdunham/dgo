#' \code{dgirt} example data: U.S. population targets
#'
#' A table giving U.S. population proportions by year for combinations of
#' demographic variables. Data are from the U.S. Census.

#' When using this table to adjust survey weights via the \code{target_data}
#' arguments of \code{\link{shape}}, note that its proportions sum to to 1
#' within years.
#'
#' @docType data
#' @name targets
#' @usage targets
#' @format A \code{data.frame} with 18,360 observations of 6 variables.
#' @seealso \url{http://www.census.gov/}
#' @examples
#' targets
NULL
