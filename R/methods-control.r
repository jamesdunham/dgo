`:=` <- data.table::`:=`
`%chin%` <- data.table::`%chin%`

# Constructor for Control
init_control <- function(item_data = item_data,
                         item_names = item_names,
                         time_name = time_name,
                         geo_name = geo_name,
                         group_names = group_names,
                         id_vars = id_vars,
                         time_filter = time_filter,
                         geo_filter = geo_filter,
                         min_t_filter = min_t_filter,
                         min_survey_filter = min_survey_filter,
                         survey_name = survey_name,
                         aggregate_data = aggregate_data,
                         aggregate_item_names = aggregate_item_names,
                         modifier_data = modifier_data,
                         modifier_names = modifier_names,
                         t1_modifier_names = t1_modifier_names,
                         standardize = standardize,
                         target_data = target_data,
                         raking = raking,
                         weight_name = weight_name,
                         proportion_name = proportion_name,
                         constant_item = constant_item,
                         ...) {
  ctrl <- new("Control",
              # item data
              item_names = item_names,
              time_name = time_name,
              geo_name = geo_name,
              group_names = group_names,
              id_vars = id_vars,
              # restrictions
              time_filter = time_filter,
              geo_filter = geo_filter,
              min_t_filter = min_t_filter,
              min_survey_filter = min_survey_filter,
              survey_name = survey_name,
              # aggregate data
              aggregate_item_names = aggregate_item_names,
              # modifier data
              modifier_names = modifier_names,
              t1_modifier_names = t1_modifier_names,
              standardize = standardize,
              # target data
              raking = raking,
              weight_name = weight_name,
              proportion_name = proportion_name,
              # modeling options
              constant_item = constant_item,
              ...)
  is_item_name <- valid_names(item_data, ctrl, 1L)
  is_item_name(c("time_name", "geo_name"))
  has_type(c("time_name", "geo_name"), item_data, ctrl)
  if (length(aggregate_data)) {
    is_agg_name <- valid_names(aggregate_data, ctrl, 1L)
    is_agg_name(c("time_name", "geo_name"))
    has_type(c("time_name", "geo_name"), aggregate_data, ctrl)
    if (!length(aggregate_item_names)) {
      is_agg_name <- valid_names(aggregate_data, NULL, 1L)
      is_agg_name("item")
      ctrl@aggregate_item_names = sort(unique(aggregate_data[["item"]]))
    }
  }

  if (length(ctrl@modifier_names)) {
    if (!length(ctrl@t1_modifier_names)) {
      ctrl@t1_modifier_names <- ctrl@modifier_names
    }
  }

  if (!length(ctrl@time_filter)) {
    ctrl@time_filter <- sort(unique(item_data[[ctrl@time_name]]))
    if (length(aggregate_data)) {
      ctrl@time_filter <- sort(unique(c(ctrl@time_filter,
                                        aggregate_data[[ctrl@time_name]])))
    }
  }

  if (!length(ctrl@geo_filter)) {
    ctrl@geo_filter <- sort(unique(as.character(item_data[[ctrl@geo_name]])))
    if (length(aggregate_data)) {
      ctrl@geo_filter <- sort(unique(c(ctrl@geo_filter,
                                        aggregate_data[[ctrl@geo_name]])))
    }
  }

  if (length(raking)) {
    if (is.list(ctrl@raking)) {
      ctrl@rake_names = unlist(lapply(ctrl@raking, all.vars))
    } else {
      ctrl@rake_names = all.vars(ctrl@raking)
    }
  }
  ctrl
}
