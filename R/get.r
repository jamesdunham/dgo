get_t <- function() {
  length(.self$filters$t)
}

get_q <- function() {
  sum(grepl("_gt", colnames(.self$tbl), fixed = TRUE))
}

get_g <- function() {
  factor_levels <- sapply(.self$tbl[, c(.self$geo, .self$groups)], nlevels)
  Reduce(`*`, factor_levels)
}

get_n = function() {
  nrow(.self$group_counts)
}

get_p = function() {
  ncol(.self$group_design_matrix)
}

get_s = function() {
  if (length(.self$ZZ) > 0) dim(.self$ZZ)[[2]]
  else NULL
}
get_h = function() {
  if (length(.self$ZZ) > 0) dim(.self$ZZ)[[3]]
  else NULL
}

get_h_prior = function() {
  if (length(.self$ZZ) > 0) dim(.self$ZZ_prior)[[3]]   
  else NULL
}
