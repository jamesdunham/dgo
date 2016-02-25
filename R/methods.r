has_hierarchy <- function() {
  length(self$modifier$tbl) > 0
}

make_WT <- function() {
  self$modifier$WT <- array(1, dim = c(self$T, self$G_hier, self$G))
}

get_names <- function() {
  nm = Map(function(i) self[[i]], names(Item$public_fields)[grep("ItemVar", Item$public_fields)])
  unique(unlist(nm))
}

test_names <- function(x) {
  stopifnot(inherits(x, "ItemVar"))
  for (s in x) {
    if (!s %in% names(self$tbl)) {
      stop(s, " is not a variable in item data")
    }
  }
}

restrict <- function() {
  self <- restrict_items(self)
  self <- restrict_modifier(self)
  self$tbl <- droplevels(self$tbl)
}

reweight <- function() {
  if (inherits(self$targets$tbl, "data.frame")) {
    self <- weight(self)
  }
}

make_gt_variables <- function() {
  gt_table <- create_gt_variables(self)
  self$tbl <- dplyr::bind_cols(self$tbl, gt_table)
}

find_missingness <- function() {
  self$MMM <- make_missingness_array(self)
}

get_group_grid <- function() {
  self$group_grid <- make_group_grid(self)
}

make_l2_only <- function() {
  self$modifier$l2_only <- make_dummy_l2_only(self)
}

make_NNl2 <- function() {
  self$modifier$NNl2 <- make_dummy_l2_counts(self)
}

make_SSl2 <- function() {
  self$modifier$SSl2 <- make_dummy_l2_counts(self)
}

make_group_design <- function() {
    self$modifier$group_design_matrix <- make_design_matrix(self)
}

make_ZZ <- function() {
  self$modifier$ZZ <- make_hierarchical_array(self)
}

make_ZZ_prior <- function() {
  self$modifier$ZZ_prior <- make_hierarchical_array(self)
}

check_groups <- function(group_grid_t) {
  for (s in self$groups) {
    if (length(levels(group_grid_t[[s]])) < 2) {
      stop("no variation in group variable ", s)
    }
  }
}

list_groups <- function() {
  self$group_grid <- make_group_grid(self)
}

list_groups_t <- function() {
  self$group_grid_t <- make_group_grid_t(self)
}

group_n <- function() {
  self$group_counts <- make_group_counts(self)
}
