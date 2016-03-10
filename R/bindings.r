gen_bind <- function(name, check_names = FALSE) {
  f = function(value) {
    if (!missing(value) && length(value) > 0) {
      private$name <- value
    } 
    if (!missing(value) && check_names == TRUE) {
      self$test_names(value)
    }
    private$name
  }
  pryr::unenclose(f)
}

bind_tbl <- function(value) {
  if (!missing(value) && length(value) > 0) {
    private$tbl_ <- value
    tbl_name <- tolower(class(self)[1])
    if (!inherits(private$tbl_, "data.frame")) {
      stop(tbl_name, " data must inherit from data.frame")
    }
    if (!all(dim(private$tbl_) > 0)) {
      stop(tbl_name, " data has an empty dimension")
    }
  }
  private$tbl_
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
    private$items_ <- value
  } 
  # with or without an argument,return the field value
  private$items_
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
  nrow(private$group_counts_)
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
  dim(self$modifier$ZZ_prior)[[3]]
}

bind_G_hier = function() {
  if (!length(self$G_hier_) > 0) {
    if (!self$has_hierarchy()) {
      hierarchical_group <- gl(1L, self$G)
      private$G_hier_ <- nlevels(hierarchical_group)
    } else {
      private$G_hier_ <- max(unlist(length(self$modifier$modifiers)), 1L)
    }
  }
  private$G_hier_
}
