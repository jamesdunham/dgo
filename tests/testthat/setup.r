min_item_call <- function(...) {
  default <- list(item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_groupless_call <- function(...) {
  default <- list(item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_modifier_call <- function(...) {
  default <- list(item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female",
                  modifier_data = states,
                  modifier_names = "prop_evangelicals",
                  t1_modifier_names = "prop_evangelicals")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_agg_call <- function(...) {
  default <- list(aggregate_data = aggregates,
                  item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

