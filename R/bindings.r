bind_tbl <- function(value) {
  if (!missing(value) && length(value) > 0) {
    private$tbl_ <- value
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
    private$tbl_
  }
}

bind_items <- function(value) {
  # if called with an arguemnt, set the field
  if (!missing(value)) {
    # stopping at attempts to assign null
    if (length(value) < 1)  {
      stop("can't assign NULL to a variable name field")
    }
    # create a new ItemVar object with the assigned value. note that the
    # initialize method of the ItemVar class will run checks on validity 
    value <- new("ItemVar", value)
    # test that the new names appear in the data
    self$test_names(value)
  }
  # with or without an argument,return the field value
  private$items_ <- value
}

bind_groups <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$control$groups <- value
      self$test_names(value)
    } else {
      self$control$groups
    }
}

bind_time <- function(value) {
    if (!missing(value) && length(value) > 0) {
      private$time_ <- value
      self$test_names(value)
    } else {
      private$time_
    }
}

bind_geo <- function(value) {
    if (!missing(value) && length(value) > 0) {
      private$geo_ <- value
      self$test_names(value)
    } else {
      private$geo_
    }
}

bind_survey <- function(value) {
    if (!missing(value) && length(value) > 0) {
      private$survey_ <- value
      self$test_names(value)
    } else {
      private$survey_
    }
}

bind_weight <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$targets$weight <- value
      self$targets$test_names(value)
    } else {
      self$targets$weight
    }
}

bind_modifiers <- function(value) {
    if (!missing(value) && length(value) > 0) {
      private$modifiers_ <- value
      self$test_names(value)
    } else {
      private$modifiers_
    }
}

bind_t1_modifiers <- function(value) {
    if (!missing(value) && length(value) > 0) {
      self$t1_modifiers_ <- value
      self$test_names(value)
    } else {
      self$t1_modifiers_
    }
}

bind_T <- function() {
  length(self$filters$time)
}

bind_Q <- function() {
  sum(grepl("_gt", colnames(self$tbl), fixed = TRUE))
}

bind_G <- function() {
  factor_levels <- sapply(self$tbl[, c(self$geo, self$groups)], nlevels)
  Reduce(`*`, factor_levels)
}

bind_N = function() {
  nrow(self$modifier$group_counts)
}

bind_P = function() {
  ncol(self$modifier$group_design_matrix)
}

bind_S = function() {
  if (length(self$modifier$ZZ) > 0) dim(self$modifier$ZZ)[[2]]
  else NULL
}
bind_H = function() {
  if (length(self$modifier$ZZ) > 0) dim(self$modifier$ZZ)[[3]]
  else NULL
}

bind_H_prior = function() {
  if (length(self$modifier$ZZ) > 0) dim(self$modifier$ZZ_prior)[[3]]
  else NULL
}

bind_G_hier = function() {
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
