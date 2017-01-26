#' \code{expand_rownames}: expand parameter descriptions in rownames
#'
#' Move rownames that describe parameters (e.g. xi[2009]) to columns.
#'
#' It should rarely be necessary to call \code{expand_rownames} directly. But
#' elements extracted from \code{\link{dgirtfit}}-class objects may have
#' rownames of the format \code{param[group1__groupK,t]} for parameters indexed
#' by group and time period, or \code{param[t]} for parameters indexed by time
#' period. \code{expand_rownames} moves this information to columns whose names
#' are given by the \code{col_names} argument. The rownames in their original
#' format will appear in another column called \code{rn}.
#'
#' @param x A table with rownames in the format \code{param[group1__groupK,t]}
#' or \code{param[t]}.
#'
#' @param time_name A name for any resulting time variable.
#'
#' @param geo_name A name for any resulting geographic variable.
#'
#' @param group_names Names for any resulting group variables.
#'
#' @return \code{x} with additional columns (see details).
#' @seealso \code{\link{dgirtfit-class}}
#' @export
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
          list(comma_split[[index_pos]][parnames == parname])]
      }
    }
  }
  if ("group_names" %in% names(x)) {
    us_split <- strsplit(x[["group_names"]], "__", fixed = TRUE)
    n_col <- max(vapply(us_split, length, integer(1L)))
    group_cols <- paste0("group_", seq.int(0, n_col - 1L))
    x[, c(group_cols) := data.table::tstrsplit(group_names, "__", fixed = TRUE)]
    if (length(geo_name)) {
      x[, c(geo_name, group_cols[1L]) := list(get(group_cols[1L]), NULL)]
    }
    if (length(group_names)) {
      x[, c(group_names, group_cols[-1L]) := list(get(group_cols[-1L]), NULL)]
    }
    x[, c("group_names") := NULL]
  }
  if (length(time_name) && "time_name" %in% names(x)) {
    names(x)[names(x) == "time_name"] <- time_name
    if (is.character(x[[time_name]])) {
      x[, c(time_name) := type.convert(x[[time_name]])]
    }
  }
  x
}
