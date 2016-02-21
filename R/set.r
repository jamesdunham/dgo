set_tbl <- function(x) {
  if (!missing(x) && length(x) > 0) {
    tbl_ <<- x
    tbl_name <- tolower(class(.self))
    if (!inherits(x, "data.frame")) {
      stop(tbl_name, " data must inherit from data.frame")
    }
    if (!all(dim(x) > 0)) {
      stop(tbl_name, " data has an empty dimension")
    } else {
      for (s in .self$get_names()) {
        if (!s %in% names(x)) {
          stop(s, " is not a variable in ", tbl_name, " data")
        }
      }
    } 
  } else {
    tbl_
  }
}

set_items <- function(x) {
    if (!missing(x)) {
      if (!length(x) < 1)  {
        if (is.character(x)) {
          x <- new("ItemVar", x)
        }
        .self$test_names(x)
      }
      items_ <<- x
    } else {
      items_
    }
}

set_groups <- function(x) {
    if (!missing(x) && length(x) > 0) {
      groups_ <<- x
      .self$test_names(x)
    } else {
      groups_
    }
}

set_time <- function(x) {
    if (!missing(x) && length(x) > 0) {
      time_ <<- x
      .self$test_names(x)
    } else {
      time_
    }
}

set_geo <- function(x) {
    if (!missing(x) && length(x) > 0) {
      geo_ <<- x
      .self$test_names(x)
    } else {
      geo_
    }
}

set_survey <- function(x) {
    if (!missing(x) && length(x) > 0) {
      survey_ <<- x
      .self$test_names(x)
    } else {
      survey_
    }
}

set_weight <- function(x) {
    if (!missing(x) && length(x) > 0) {
      weight_ <<- x
      .self$test_names(x)
    } else {
      weight_
    }
}

