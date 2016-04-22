# summary(toy_dgirt_in)
summary.dgirtIn <- function(object) {
  cat('<dgirtIn> object:\n',
      object$Q, ngettext(object$Q, "item", "items"),
        paste0('(', length(object$gt_items), ' after discretizing)\n'),
      prod(dim(object$MMM)), 'grouping variable combinations', paste0('(', sum(!object$MMM)), 'observed)\n',
      # FIXME: time_observed and geo_observed not implemented
      object$T, 'time periods', paste0('(', length(object$time_observed)), 'observed)\n',
      length(object$geo_observed), 'local geographic',
        ngettext(length(object$geo_observed), 'area', 'areas'), '\n',
      length(object$control@group_names), 'other grouping',
        ngettext(length(object$control@group_names), 'variable:', 'variables:'),
        paste(object$control@group_names, sep = ","), '\n',
    object$P, 'hierarchical parameters\n',
    length(object$control@modifier_names), 'hierarchical',
        ngettext(length(object$control@modifier_names), 'modifier', 'modifiers'), '\n')
}

setGeneric("get_item_names", signature = "x",
           function(x) standardGeneric("get_item_names"))
#' @rdname dgirtIn-class
#' Get Items Names in DGIRT Data.
#'
#' @param x An object of class `dgirtIn` as returned by `shape`.
#' @return A list of item names.
#' @examples
#' get_item_names(toy_dgirt_in)
setMethod("get_item_names", c("x" = "dgirtIn"),
          function(x) {
            list(item_data = x$control@item_names,
                 aggregate_data = x$control@aggregate_item_names)
          })

setGeneric("get_n", signature = c("x", "by", "aggregate_name"),
           function(x, by = NULL, aggregate_name = NULL)
             standardGeneric("get_n"))
#' @rdname dgirtIn-class
#' Count Respondents in DGIRT Data.
#'
#' @param x An object of class `dgirtIn` as returned by `shape`.
#' @param by The name of a grouping variable.
#' @param aggregate_name If specified `get_n` will operate on the table passed
#' to `shape` as `aggregate_data` instead of on the individual data and count
#' nonmissingness in the given variable.
#' 
#' @examples
#' # respondents count
#' get_n(toy_dgirt_in)
#'
#' # respondent count by year
#' get_n(toy_dgirt_in, by = "year")
#'
#' # respondent count by survey identifier
#' get_n(toy_dgirt_in, by = "source")
#'
#' # specify `aggregate_name` to get counts of a variable in the aggregate data
#' get_n(toy_dgirt_in, aggregate_name = "race", by = "year")
#'
#' @seealso `\link{get_item_n}, \link{get_item_names}`
# toy_dgirt_in$aggregate_data = aggregate_data
# get_n(toy_dgirt_in, "item", aggregate_data = TRUE)
# get_n(toy_dgirt_in, "item", by = "year", aggregate_data = TRUE)
setMethod("get_n", c("x" = "dgirtIn"),
  function(x, by = NULL, aggregate_name = NULL) {
    if (!length(aggregate_name)) {
        n <- x$item_data[, .(n = .N), keyby = by]
    } else {
      if (!length(x$aggregate_data))
        stop("Found no aggregate data")
      if (!aggregate_name %chin% names(x$aggregate_data))
        stop(aggregate_name, "is not a name in aggregate data")
      n <- x$aggregate_data[, .(n = sum(n_grp, na.rm = TRUE)),
                            keyby = c(aggregate_name, by)]
      if (length(by)) {
        f <- as.formula(paste(by, aggregate_name, sep = "~"))
        n <- data.table::dcast(n, f, fun.aggregate = sum, value.var = "n")
      }
      # TODO: make sure that aggregate_data is restricted
      n[, (setdiff(names(n), by)) := lapply(.SD, function(k) replace(k, is.na(k), 0L)),
             .SDcols = setdiff(names(n), by)]
    }
    copy(n)
  })

setGeneric("get_item_n", signature = c("x", "by", "aggregate_data"),
           function(x, by = NULL, aggregate_data = FALSE) standardGeneric("get_item_n"))
#' @rdname dgirtIn-class
# get_item_n(toy_dgirt_in)
# get_item_n(toy_dgirt_in, by = "year")
# get_item_n(toy_dgirt_in, aggregate_data = TRUE)
# get_item_n(toy_dgirt_in, by = "year", aggregate_data = TRUE)
setMethod("get_item_n", c("x" = "dgirtIn"),
  function(x, by = NULL, aggregate_data = FALSE) {
    if (!isTRUE(aggregate_data)) {
      n <- x$item_data[, lapply(.SD, function(z) sum(!is.na(z))),
                       .SDcols = x$control@item_names, keyby = by]
    } else {
    if (!length(x$aggregate_data))
      stop("Found no aggregate data")
    n <- x$aggregate_data[, .(n = sum(n_grp)), keyby = c("item", by)]
    if (length(by)) {
      f <- as.formula(paste(by, "item", sep = "~"))
      n <- data.table::dcast(n, f, fun.aggregate = sum, value.var = "n")
    }
    n[, (setdiff(names(n), by)) := lapply(.SD, function(k) replace(k, is.na(k), 0L)),
      .SDcols = setdiff(names(n), by)]
  }
  copy(n)
})
