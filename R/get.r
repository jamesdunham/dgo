get_T <- function() {
  length(self$filters$t)
}

get_Q <- function() {
  sum(grepl("_gt", colnames(self$tbl), fixed = TRUE))
}

get_G <- function() {
  factor_levels <- sapply(self$tbl[, c(self$geo, self$groups)], nlevels)
  Reduce(`*`, factor_levels)
}

get_N = function() {
  nrow(self$group_counts)
}

get_P = function() {
  ncol(self$group_design_matrix)
}

get_S = function() {
  if (length(self$ZZ) > 0) dim(self$ZZ)[[2]]
  else NULL
}
get_H = function() {
  if (length(self$ZZ) > 0) dim(self$ZZ)[[3]]
  else NULL
}

get_H_prior = function() {
  if (length(self$ZZ) > 0) dim(self$ZZ_prior)[[3]]   
  else NULL
}

get_G_hier = function() {
  if (!length(self$modifier$G_hier_)) { 
    if (!inherits(self$tbl, "data.frame")) {
      hierarchical_group <- gl(1L, self$G)
      self$modifier$G_hier_ <- nlevels(hierarchical_group)
    } else {
      self$modifier$G_hier_ <- max(unlist(length(self$modifier$modifiers)), 1L)
    }
    self$modifier$G_hier_
  }
}
