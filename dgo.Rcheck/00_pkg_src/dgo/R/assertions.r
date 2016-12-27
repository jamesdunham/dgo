assert <- assertthat::assert_that

all_equal <- function(x, y) {
  isTRUE(all.equal(x, y))
}

assertthat::on_failure(all_equal) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y), " are not all equal")
}

equal_length = function(x, y) {
  assertthat::are_equal(length(x), length(y))
}

assertthat::on_failure(equal_length) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y), " have different lengths")
}

is_subset <- function(x, y) {
  all(unique(x) %in% unique(y))
}

assertthat::on_failure(is_subset) <- function(call, env) {
  paste0(deparse(call$x), " is not a subset of ", deparse(call$y))
}

not_empty <- function(x) {
  assertthat::not_empty(x)
}

assertthat::on_failure(not_empty) <- function(call, env) {
  paste0("empty dimension in ", deparse(call$x))
}

has_all_names <- function(table, names, suggestion = NULL) {
  assertthat::assert_that(assertthat::not_empty(table), assertthat::not_empty(names))
  which_has_name <- vapply(names, function(i) {
    assertthat::has_name(table, i)
  }, logical(1), USE.NAMES = FALSE)
  all(which_has_name)
}

assertthat::on_failure(has_all_names) <- function(call, env) {
  paste0("not all ", call$names, " are names in ", deparse(call$table))
}

all_strings <- function(x) {
  if (length(x) < 1) return(FALSE)
  which_strings <- vapply(x, function(i) {
    assertthat::is.string(i) && nchar(i) > 0
  }, logical(1))
  all(which_strings)
}

assertthat::on_failure(all_strings) <- function(call, env) {
  paste0(deparse(call$x), " are not all positive-length strings")
}
