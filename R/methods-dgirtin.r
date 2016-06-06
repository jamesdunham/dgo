setGeneric("summary")
#' Summarize DGIRT Data.
#'
#' @param x An object of class \code{dgirtIn} as returned by \code{shape}.
#'
#' @param ... Unused.
#'
#' @param object An object of class \code{dgirtIn} as returned by \code{shape}.
#'
#' @include class-dgirtin.r
#' @include require_namespace.r
#' @rdname dgirtin-class
#' @export
setMethod("summary", c(object = "dgirtIn"),
          function(object, ...) {
            cat("Items:\n")
            print(c(object$control@item_names,
                    object$control@aggregate_item_names))
            cat("Respondents:\n")
            cat("  ", format(get_n(object), big.mark = ","), "in `item_data` (unadjusted)\n")
            if (length(object$aggregate_data))
              cat("  ", format(sum(get_n(object, aggregate_name = "item")$n),big.mark = ","),
                  "in `aggregate_data (design-effect adjusted) `\n")
            cat("Grouping variables:\n")
            print(c(object$control@time_name,
                    object$control@geo_name,
                    object$control@group_names))
            cat("Time periods:\n")
            print(object$control@time_filter)
            cat("Local geographic areas:\n")
            print(object$control@geo_filter)
            cat("Hierarchical parameters:\n")
            print(object$hier_names)
            cat("Modifiers of hierarchical parameters:\n")
            print(object$control@modifier_names)
            cat("Constants:\n")
            print(c(Q = object$Q, T = object$T, P = object$P, N = object$N, G = object$G,
                    H = object$H, D = object$D))
          })

setGeneric("print")
setMethod("print", c("x" = "dgirtIn"),
          function(x, ...) summary(x))

setGeneric("get_item_names", signature = "x",
           function(x) standardGeneric("get_item_names"))
#' Get Items Names in DGIRT Data.
#'
#' @return A list of item names.
#' @examples
#' get_item_names(toy_dgirt_in)
#' @include class-dgirtin.r
#' @rdname dgirtin-class
#' @aliases get_item_names
#' @export
setMethod("get_item_names", c("x" = "dgirtIn"),
          function(x) {
            list(item_data = x$control@item_names,
                 aggregate_data = x$control@aggregate_item_names)
          })

setGeneric("get_n", signature = c("x", "by", "aggregate_name"),
           function(x, by = NULL, aggregate_name = NULL)
             standardGeneric("get_n"))
#' Count Respondents in DGIRT Data.
#'
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
#' @seealso `\link{get_item_n}, \link{get_item_names}`
#' @include class-dgirtin.r
#' @include require_namespace.r
#' @aliases get_n
#' @rdname dgirtin-class
#' @export
setMethod("get_n", c("x" = "dgirtIn"),
  function(x, by = NULL, aggregate_name = NULL) {
    if (!length(aggregate_name)) {
        n <- x$item_data[, list(n = .N), keyby = by]
    } else {
      if (!length(x$aggregate_data))
        stop("Found no aggregate data")
      if (!aggregate_name %chin% names(x$aggregate_data))
        stop(aggregate_name, "is not a name in aggregate data")
      n <- x$aggregate_data[, list(n = sum(get("n_grp"), na.rm = TRUE)),
                            keyby = c(aggregate_name, by)]
      if (length(by)) {
        f <- as.formula(paste(by, aggregate_name, sep = "~"))
        n <- data.table::dcast(n, f, fun.aggregate = sum, value.var = "n")
      }
      # TODO: make sure that aggregate_data is restricted
      n[, (setdiff(names(n), by)) := lapply(.SD, function(k) replace(k, is.na(k), 0L)),
             .SDcols = setdiff(names(n), by)]
    }
    data.table::copy(n)
  })

setGeneric("get_item_n", signature = c("x", "by", "aggregate_data"),
           function(x, by = NULL, aggregate_data = FALSE) standardGeneric("get_item_n"))

#' Count Respondents for Items in DGIRT Data
#' @include class-dgirtin.r
#' @include require_namespace.r
#' @rdname dgirtin-class
#' @examples
#' get_item_n(toy_dgirt_in)
#' get_item_n(toy_dgirt_in, by = "year")
#' @aliases get_item_n
#' @param aggregate_data If specified `get_n` will operate on the table passed
#' to `shape` as `aggregate_data` instead of on the individual data.
#' @export
setMethod("get_item_n", c("x" = "dgirtIn"),
  function(x, by = NULL, aggregate_data = FALSE) {
    if (!isTRUE(aggregate_data)) {
      n <- x$item_data[, lapply(.SD, function(z) sum(!is.na(z))),
                       .SDcols = x$control@item_names, keyby = by]
    } else {
    if (!length(x$aggregate_data))
      stop("Found no aggregate data")
    n <- x$aggregate_data[, list(n = sum(get("n_grp"))), keyby = c("item", by)]
    if (length(by)) {
      f <- as.formula(paste(by, "item", sep = "~"))
      n <- data.table::dcast(n, f, fun.aggregate = sum, value.var = "n")
    }
    n[, (setdiff(names(n), by)) := lapply(.SD, function(k) replace(k, is.na(k), 0L)),
      .SDcols = setdiff(names(n), by)]
  }
  data.table::copy(n)
})

#' Show Summary of DGIRT Data
#' @include class-dgirtin.r
#' @include require_namespace.r
#' @rdname dgirtin-class
#' @export
setMethod("show", c("dgirtIn"), function(object) summary(object))
