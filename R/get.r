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
  dim(.self$ZZ)[[2]]
}
get_h = function() {
  dim(.self$ZZ)[[3]]
}

get_h_prior = function() {
  dim(.self$ZZ_prior)[[3]]   
}
