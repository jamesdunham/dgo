#' \code{expand_rownames}: extract parameter descriptions in rownames
#'
#' Move rownames that describe parameters (e.g. xi[2009]) to columns.
#'
#' Tables of \code{\link{dgirtfit}}-class \code{\link{dgirt}} output  will often
#' have descriptive rownames of the format \code{param[group1__groupK,t]} for
#' parameters indexed by group and time period, or \code{param[t]} for
#' parameters indexed by time period.  \code{expand_rownames} is a convenience
#' function for moving this information to columns in those tables whose names
#' are given by the \code{col_names} argument. The rownames in their original
#' format will appear in the column \code{rn}.
#'
#' @param x A table with rownames in the format \code{param[group1__groupK,t]}
#' or \code{param[t]}.
#'
#' @param col_names A character vector of column names to be created, in the
#' same order that their values appear in the rownames.
#'
#' @return \code{x} with additional columns (see details).
#'
#' @examples
#' data(toy_dgirtfit)
#' tb_means <- get_posterior_mean(toy_dgirtfit, name = FALSE)
#' # rownames are e.g. "theta_bar[CO__black,2011]"
#' tb_means <- expand_rownames(tb_means, time_name = "year", geo_name = "state", group_names = "race")
#' # result has columns state, race, year (and original rownames in rn)
#' head(tb_means)
#'
#' # similarly for a parameter indexed t
#' xi_means <- get_posterior_mean(toy_dgirtfit, pars = 'xi')
#' xi_means <- get_posterior_mean(toy_dgirtfit, pars = 'gamma')
#' xi_means <- expand_rownames(xi_means, time_name = "year")
#' head(xi_means)
#' @seealso \code{\link{dgirtfit-class}}
#' @include data-toy_dgirtfit.r
expand_rownames <- function(x, time_name, geo_name, group_names) {
  if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE,
                                       rownames = rownames(x))
  x <- data.table::copy(data.table::setDT(x, keep.rownames = TRUE))
  if (!"rn" %in% names(x)) stop("After setDT(x, keep.rownames = TRUE), ",
                                "rownames couldn't be found. Did x ",
                                "have rownames?")
  indexes <- gsub('.*\\[([A-Za-z0-9,_]+)\\].*', '\\1', x[["rn"]])
  parnames <- gsub('(.*)\\[[A-Za-z0-9,_]+\\].*', '\\1', x[["rn"]])
  comma_split <- data.table::tstrsplit(indexes, c(","))
  for (parname in unique(parnames)) {
    # index_names is a list for looking up the names of parameter indexes
    for (i in index_names[[parname]]) {
      if (length(i)) {
        index_pos <- which(index_names[[parname]] == i)
        x[parnames == parname, c(i) :=
          list(comma_split[[index_pos]][parnames == parname]), with = FALSE]
      }
    }
  }
  if ("group_names" %in% names(x)) {
    us_split <- strsplit(x[["group_names"]], "__", fixed = TRUE)
    n_col <- max(vapply(us_split, length, integer(1L)))
    group_cols <- paste0("group_", seq.int(0, n_col - 1L))
    x[, c(group_cols) := data.table::tstrsplit(group_names, "__", fixed = TRUE)]
    if (length(geo_name)) {
      x[, c(geo_name, group_cols[1L]) := list(get(group_cols[1L]), NULL),
        with = FALSE]
    }
    if (length(group_names)) {
      x[, c(group_names, group_cols[-1L]) := list(get(group_cols[-1L]), NULL),
        with = FALSE]
    }
    x[, c("group_names") := NULL, with = FALSE]
  }
  if (length(time_name) && "time_name" %in% names(x)) {
    names(x)[names(x) == "time_name"] <- time_name
    x[, c(time_name) := type.convert(x[[time_name]]), with = FALSE]
  }
  data.table::copy(x)
}

