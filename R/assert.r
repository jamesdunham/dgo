assert_is_flag = function(x) {
  assertthat::assert_that(assertthat::is.flag(x))
}
assert_is_string = function(x) {
  assertthat::assert_that(assertthat::is.string(x))
}
assert_has_name = function(x, which) {
  assertthat::assert_that(assertthat::has_name(x, which))
}
assert_has_attr = function(x, which) {
  assertthat::assert_that(assertthat::has_attr(x, which))
}
assert_is_count = function(x) {
  assertthat::assert_that(assertthat::is.count(x))
}
assert_are_counts = Vectorize(assertthat::is.count)
assert_are_equal = function(x, y) {
  assertthat::assert_that(assertthat::are_equal(x, y))
}
assert_not_empty = function(x) {
  assertthat::assert_that(assertthat::not_empty(x))
}
assert_no_na = function(x) {
  assertthat::assert_that(assertthat::noNA(x))
}
assert_is_dir = function(path) {
  assertthat::assert_that(assertthat::is.dir(path))
}
assert_is.writeable = function(path) {
  assertthat::assert_that(assertthat::is.writeable(), path)
}
assert_has_extension = function(path, extension) {
  assertthat::assert_that(assertthat::has_extension(path, extension))
}
assert_equal_length = function(x, y) {
  assertthat::assert_that(assertthat::are_equal(length(x), length(y)))
}
assert_equal = function(x, y) {
  assertthat::assert_that(assertthat::are_equal(x, y))
}

equal_length <- function(x, y) {
  assertthat::assert_that(identical(length(x), length(y)))
}

is_subset <- function(x, y) {
  assertthat::assert_that(all(unique(x) %in% unique(y)))
}

is_positive <- function(x) {
  assertthat::assert_that(is.numeric(x))
  x > 0
}
equal_length <- function(x, y) {
  assertthat::assert_that(identical(length(x), length(y)))
}

is_subset <- function(x, y) {
  assertthat::assert_that(all(unique(x) %in% unique(y)))
}

is_positive <- function(x) {
  assertthat::assert_that(is.numeric(x))
  x > 0
}

none_empty <- function(x) {
  assertthat::assert_that(assertthat::not_empty(x))
  for (element in x) {
    assertthat::assert_that(assertthat::not_empty(element))
  }
  return(TRUE)
}
assertthat::on_failure(none_empty) <- function(call, env) {
  paste0(deparse(call$x), " has empty element")
}

has_all_names <- function(x, x_elements) {
  assertthat::assert_that(assertthat::not_empty(x))
  assertthat::assert_that(assertthat::not_empty(x_elements))
  for (element in x_elements) {
    assertthat::assert_that(assertthat::has_name(x, element))
  }
  return(TRUE)
}

all_valid_strings <- function(x) {
  if (!assertthat::not_empty(x)) {
    return(FALSE)
  } else {
  valid_strings <- vapply(x, function(element) {
    assertthat::is.string(element) &&
      assertthat::see_if(nchar(element) > 0)
    }, FALSE)
  }
  all(valid_strings)
}
assertthat::on_failure(all_valid_strings) <- function(call, env) {
  paste0("all elements of object '", deparse(call$x),
    "' should be positive-length strings")
}

is_valid_string <- function(x) {
  assertthat::is.string(x)
  assertthat::see_if(nchar(x) > 0)
}
assertthat::on_failure(is_valid_string) <- function(call, env) {
  paste0("object '", deparse(call$x),
    "' should be a positive-length string")
}
