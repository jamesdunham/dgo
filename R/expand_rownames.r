#' Expand Descriptive Rownames.
#'
#' Move rownames that describe parameters (e.g. xi[2009]) to columns.
#'
#' Tables of `dgirt` output will often have descriptive rownames of the format
#' `param[group1__groupK,t]` for parameters indexed by group and time period, or
#' `param[t]` for parameters indexed by time period. `expand_rownames` is a
#' convenience function for moving this information to columns in those tables
#' whose names are given by the `col_names` argument. The rownames in their
#' original format will appear in the column `rn`.
#' @param x A table with rownames in the format `param[group1__groupK,t]` or
#' `param[t]`.
#' @param col_names A character vector of column names to be created, in the
#' same order that their values appear in the rownames. 
#' @return `x` with additional columns (see details).
#' @examples
#' data(toy_dgirtfit)
#' tb_means <- get_posterior_mean(toy_dgirtfit, pars = 'theta_bar')
#' # rownames are e.g. "theta_bar[CO__black,2011]"
#' tb_means <- expand_rownames(tb_means, c("state", "race", "year"))
#' # result has columns state, race, year (and original rownames in rn)
#' head(tb_means)
#' # similarly for a parameter indexed t
#' xi_means <- get_posterior_mean(toy_dgirtfit, pars = 'xi')
#' xi_means <- expand_rownames(xi_means, col_names = c("year"))
#' head(xi_means)
#' @include data-toy_dgirtfit.r
expand_rownames <- function(x, col_names) {
  if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE,
                                       rownames = rownames(x))
  x <- copy(setDT(x, keep.rownames = TRUE))
  rn <- gsub('.*\\[([A-Za-z0-9,_]+)\\].*', '\\1', x[, rn])
  comma_split <- data.table::tstrsplit(rn, c(","))
  if (length(comma_split) > 1L) {
    # group columns (assume only one comma)
    us_split <- data.table::tstrsplit(comma_split[[-2L]], "__")
    if (1L + length(us_split) != length(col_names)) 
      stop("\"col_names\" is length ", length(col_names), " but expanded ",
           "rownames are length ", 1L + length(us_split))
    else x[, (col_names[-length(col_names)]) := us_split]
  } else if (length(col_names) != 1L) {
      stop("\"col_names\" is length ", length(col_names), " but expanded ",
           "rownames are length 1")
  }
  # time column
  x[, (col_names[length(col_names)]) := comma_split[[length(comma_split)]]]
  copy(x)
}
