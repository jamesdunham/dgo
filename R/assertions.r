assert <- assertthat::assert_that

all_equal <- function(x, y) {
  isTRUE(all.equal(x, y))
}

assertthat::on_failure(all_equal) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y), " are not all equal")
}

is_flag <- function(x) {
  assertthat::is.flag(x)
}

assertthat::on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a length-one logical vector")
}

is_string = function(x) {
  assertthat::is.string(x)
}

assertthat::on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a length-one character vector")
}

has_var = function(x, which, name = NULL) {
  assertthat::has_name(x, which)
}

assertthat::on_failure(has_var) <- function(call, env) {
  if (length(call$name) > 0) {
    paste0(deparse(call$which), " is not a variable in ", call$name)
  } else {
    paste0(deparse(call$which), " is not a variable in ", deparse(call$x))
  }
}

is_count = function(x) {
  assertthat::is.count(x)
}

assertthat::on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a positive integer")
}

all_counts <- function(x) {
  which_counts <- vapply(x, function(i) {
    assertthat::is.count(i)
  }, logical(1), USE.NAMES = FALSE)
  all(which_counts)
}

assertthat::on_failure(all_counts) <- function(call, env) {
  paste0(deparse(call$x), " are not all positive integers")
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

is_positive <- function(x) {
  is.numeric(x) && x > 0
}

assertthat::on_failure(is_positive) <- function(call, env) {
  paste0(deparse(call$x), " is not a positive number")
}

is_numeric <- function(x) {
  is.numeric(x) && length(x) > 0
}

assertthat::on_failure(is_numeric) <- function(call, env) {
  paste0(deparse(call$x), " is not a positive-length numeric or integer vector")
}

not_empty <- function(x) {
  assertthat::not_empty(x)
}

assertthat::on_failure(not_empty) <- function(call, env) {
  paste0("empty dimension in ", deparse(call$x))
}

none_empty <- function(x) {
  assertthat::not_empty(x)
  which_not_empty <- vapply(x, function(i) {
    assertthat::not_empty(i)
  }, logical(1), USE.NAMES = FALSE)
  all(which_not_empty)
}

assertthat::on_failure(none_empty) <- function(call, env) {
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

names_match <- function(x, y) {
  identical(names(x), names(y))
}

assertthat::on_failure(names_match) <- function(call, env) {
  paste0("names differ between ", deparse(call$x), " and ", deparse(call$y))
}

names_subset <- function(x, y) {
  is_subset(names(x), names(y))
}

assertthat::on_failure(names_subset) <- function(call, env) {
  paste0("names in ", deparse(call$x), " are not a subset of those in ", deparse(call$y))
}

all_in <- function(x, set) {
  all(x %in% set)
}

assertthat::on_failure(all_in) <- function(call, env) {
  paste0(deparse(call$set), " does not include all ", deparse(call$x))
}

none_in <- function(x, set) {
  !any(x %in% set)
}

assertthat::on_failure(none_in) <- function(call, env) {
  paste0(deparse(call$set), " contains ", deparse(call$x))
}

has_length <- function(x, len) {
  identical(length(x), as.integer(len))
}

assertthat::on_failure(has_length) <- function(call, env) {
  paste0(deparse(call$x), " does not have length ", deparse(call$len))
}
