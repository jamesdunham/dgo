item_level_ <- function(vars, data, ...) {
  f = vars_to_formula(vars)
  item_level(f, data, ...)
}

item_level <- function(f, data, ...) {
  res <- level(f, data, ...)
  attr(res, "dgirt_items") <- TRUE
  # attr(res, "items") <- get_lhs(f)
  # attr(res, "vars") <- get_rhs(f)
  return(res)
}

level <- function(f, data, ...) {
  assertthat::assert_that(inherits(f, "formula"))
  assertthat::assert_that(is.data.frame(data))
  # TODO: assertthat terms in f exist in data to improve on model.frame's 'not
  # an object' error message
  res = model.frame(f, data)
  attr(res, "dgirt_level") <- TRUE
  return(res)
}

level_ = function(vars, data, ...) {
  f = vars_to_formula(vars)
  level(f, data, ...)
}

vars_to_formula <- function(vars) {
  formula(paste0("~ ", paste0(vars, collapse = " + ")))
}

is_level = function(x) {
  assertthat::assert_that(is.data.frame(x))
  level_attr = attr(x, "dgirt_level")
  if (length(level_attr) < 1 || !isTRUE(level_attr)) {
    return(FALSE)
  }
  return(TRUE)
}

is_item_level = function(x) {
  if (!is_level(x)) {
    return(FALSE)
  }
  items_attr = attr(x, "dgirt_items")
  if (length(items_attr) < 1 || !isTRUE(items_attr)) {
    return(FALSE)
  }
  return(TRUE)
}

get_item_levels = function(...) {
  d = list(...)
  d[sapply(d, is_item_level)]
}

get_levels = function(...) {
  d = list(...)
  d[sapply(d, is_level)]
}

  # TODO: assertthat the response variables in hierarchical data appear in the
  # individual data
  # 1. check for dgirt_items attribute; these are our items
  # NOTE: can pass survey data separately (!)
  # 2. check that (factor) levels of modeled params appear in dgirt_levels data
  # TODO: NSE in level()?

has_response_var = function(f) {
  assert(inherits(f, "formula"))
  assert_has_attr(terms.formula(f), "response")
  f_response = attr(terms.formula(f), "response")
  if (length(f_response) < 1 || identical(f_response, 0L)) {
    return(FALSE)
  } else if (identical(f_response, 1L)) {
    return(TRUE)
  } else {
    warning("unexpected has_response_var result")
    return(FALSE)
  }
}

get_lhs = function(f) {
  assert(has_response_var(f))
  terms(f)[[2]]
}
