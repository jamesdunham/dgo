suppressMessages({
  aggregate_data <- readRDS("~/projects/dgirt/aggregate_data.Rds")

  min_item_call <<- function(...) {
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

  min_modifier_call <<- function(...) {
    default <- list(modifier_data = dgirt::states,
                    modifier_names = "prop_evangelicals",
                    t1_modifier_names = "prop_evangelicals")
    dots <- list(...)
    dots <- c(dots, default[!names(default) %in% names(dots)])
    invisible(do.call(min_item_call, dots))
  }

  min_agg_call <<- function(...) {
    default <- list(aggregate_data = aggregate_data,
                    aggregate_item_names = unique(aggregate_data$item))
    dots <- list(...)
    dots <- c(dots, default[!names(default) %in% names(dots)])
    invisible(do.call(min_item_call, dots))
  }

  as_list <<- function(x, ...) {
    data(toy_dgirt_in)
    default <- list(shaped_data = get("toy_dgirt_in"),
                    separate_t = FALSE,
                    delta_tbar_prior_mean = 0.5,
                    delta_tbar_prior_sd = 0.5,
                    innov_sd_delta_scale = 2.5,
                    innov_sd_theta_scale = 2.5)
    dots <- list(...)
    dots <- c(dots, default[!names(default) %in% names(dots)])
    invisible(do.call(x$as_list, dots))
  }

  min_wrangle_call <<- function(...) {
    default_data <- list(data = list(level1 = opinion),
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

  min_wrangle_mod_call <<- function(...) {
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

  min_wrangle_agg_call <<- function(...) {
    default <- list(data = list(level1 = dgirt::opinion,
                                aggregates = aggregate_data),
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

  load('min_item_ns.Rdata', .GlobalEnv)
  load('wrangle_min_item_ns.Rdata', .GlobalEnv)
  load('wrangle_agg_item_ns.Rdata', .GlobalEnv)
  d_min <<- min_item_call()
  d_mod <<- min_modifier_call()

})
