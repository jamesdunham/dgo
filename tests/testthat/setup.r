min_item_call <- function(...) {
  default <- list(item_data = dgirt::opinion,
                  item_names = "Q_cces2006_abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female",
                  survey_name = "source",
                  weight_name = "weight")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_groupless_call <- function(...) {
  default <- list(item_data = dgirt::opinion,
                  item_names = "Q_cces2006_abortion",
                  time_name = "year",
                  geo_name = "state",
                  survey_name = "source",
                  weight_name = "weight")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_modifier_call <- function(...) {
  default <- list(item_data = dgirt::opinion,
                  item_names = "Q_cces2006_abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female",
                  survey_name = "source",
                  weight_name = "weight",
                  modifier_data = dgirt::states,
                  modifier_names = "prop_evangelicals",
                  t1_modifier_names = "prop_evangelicals")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_agg_call <- function(...) {
  default <- list(aggregate_data = aggregates,
                  aggregate_item_names = unique(aggregates$item),
                  item_data = dgirt::opinion,
                  item_names = "Q_cces2006_abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female",
                  survey_name = "source",
                  weight_name = "weight")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_wrangle_call <- function(...) {
  default <- list(data = list(level1 = opinion),
                       vars = list(items = "Q_cces2006_abortion",
                                   time_id = "year",
                                   geo_id = "state",
                                   groups = "female",
                                   survey_id = "source",
                                   survey_weight = "weight"))
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(dgirtlegacy::wrangle, dots))
}

min_wrangle_mod_call <- function(...) {
  default <- list(data = list(level1 = opinion,
                                   level2 = states),
                       vars = list(items = "Q_cces2006_abortion",
                                   time_id = "year",
                                   geo_id = "state",
                                   groups = "female",
                                   survey_id = "source",
                                   survey_weight = "weight",
                                   level2_modifiers = "prop_evangelicals",
                                   level2_period1_modifiers = "prop_evangelicals"))
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(dgirtlegacy::wrangle, dots))
}

min_wrangle_agg_call <- function(...) {
  default <- list(data = list(level1 = opinion,
                              aggregates = aggregates[aggregates$year %in% 2006:2010, ]),
                  vars = list(items = "Q_cces2006_abortion",
                              time_id = "year",
                              geo_id = "state",
                              groups = "female",
                              survey_id = "source",
                              survey_weight = "weight"))
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(dgirtlegacy::wrangle, dots))
}
