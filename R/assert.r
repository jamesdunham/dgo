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

is_valid_string <- function(x) {
  assertthat::is.string(x)
  assertthat::see_if(nchar(x) > 0)
}

assertthat::on_failure(none_empty) <- function(call, env) {
  paste0(deparse(call$x), " has empty element")
}

assertthat::on_failure(all_valid_strings) <- function(call, env) {
  paste0("all elements of object '", deparse(call$x),
    "' should be positive-length strings")
}

assertthat::on_failure(is_valid_string) <- function(call, env) {
  paste0("object '", deparse(call$x),
    "' should be a positive-length string")
}
