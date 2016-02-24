set_tbl <- function(value) {
  if (!missing(value) && length(value) > 0) {
    self$tbl_ <- value
    tbl_name <- tolower(class(self)[1])
    if (!inherits(value, "data.frame")) {
      stop(tbl_name, " data must inherit from data.frame")
    }
    if (!all(dim(value) > 0)) {
      stop(tbl_name, " data has an empty dimension")
    # } else {
    #   for (s in self$get_names()) {
    #     if (!s %in% names(value)) {
    #       stop(s, " is not a variable in ", tbl_name, " data")
    #     }
    #   }
    } 
  } else {
    self$tbl_
  }
}

set_items <- function(value) {
    if (!missing(value)) {
      if (!length(value) < 1)  {
        value <- new("ItemVar", value)
        self$test_names(value)
      }
      self$items_ <- value
    } else {
      self$items_
    }
}

set_groups <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$groups_ <- value
      self$test_names(value)
    } else {
      self$groups_
    }
}

set_time <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$time_ <- value
      self$test_names(value)
    } else {
      self$time_
    }
}

set_geo <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$geo_ <- value
      self$test_names(value)
    } else {
      self$geo_
    }
}

set_survey <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$survey_ <- value
      self$test_names(value)
    } else {
      self$survey_
    }
}

set_weight <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$weight_ <- value
      self$test_names(value)
    } else {
      self$weight_
    }
}

set_modifiers <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$modifiers_ <- value
      self$test_names(value)
    } else {
      self$modifiers_
    }
}

set_t1_modifiers <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$t1_modifiers_ <- value
      self$test_names(value)
    } else {
      self$t1_modifiers_
    }
}
