check_groups <- function(group_grid_t) {
  for (s in self$groups) {
    if (length(unique(group_grid_t[[s]])) < 2) {
      stop("no variation in group variable ", s)
    }
  }
}

has_hierarchy <- function() {
  length(self$modifier$tbl) > 0
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
