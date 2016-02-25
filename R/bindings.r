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
      self$control$groups <- value
      self$test_names(value)
    } else {
      self$control$groups
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
      self$targets$weight <- value
      self$targets$test_names(value)
    } else {
      self$targets$weight
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

get_T <- function() {
  length(self$filters$time)
}

get_Q <- function() {
  sum(grepl("_gt", colnames(self$tbl), fixed = TRUE))
}

get_G <- function() {
  factor_levels <- sapply(self$tbl[, c(self$geo, self$groups)], nlevels)
  Reduce(`*`, factor_levels)
}

get_N = function() {
  nrow(self$modifier$group_counts)
}

get_P = function() {
  ncol(self$modifier$group_design_matrix)
}

get_S = function() {
  if (length(self$modifier$ZZ) > 0) dim(self$modifier$ZZ)[[2]]
  else NULL
}
get_H = function() {
  if (length(self$modifier$ZZ) > 0) dim(self$modifier$ZZ)[[3]]
  else NULL
}

get_H_prior = function() {
  if (length(self$modifier$ZZ) > 0) dim(self$modifier$ZZ_prior)[[3]]
  else NULL
}

get_G_hier = function() {
  if (!length(self$modifier$G_hier_) > 0) {
    if (!self$has_hierarchy()) {
      hierarchical_group <- gl(1L, self$G)
      self$modifier$G_hier_ <- nlevels(hierarchical_group)
    } else {
      self$modifier$G_hier_ <- max(unlist(length(self$modifier$modifiers)), 1L)
    }
  }
  self$modifier$G_hier_
}
