check_targets <- function(target_data, ctrl) {
  if (!length(target_data))
    return(TRUE)
  if (!all(dim(target_data) > 0))
    stop("not all dimensions of target data are positive")
  if (!ctrl@time_name %chin% names(target_data))
    stop("\"time_name\" should give a variable name in target data")
  if (!ctrl@geo_name %chin% names(target_data))
    stop("\"geo_name\" should give a variable name in target data")
  if (!all(ctrl@group_names %chin% names(target_data)))
    stop("\"group_names\" should give one or more variable names in target data")
  if (length(ctrl@target_proportion_name) != 1 || !ctrl@target_proportion_name %chin% names(target_data))
    stop("\"target_proportion_name\" should give a single variable name in target data")
  if (!length(ctrl@strata_names) > 0L || !all(ctrl@strata_names %chin% names(target_data)))
    stop("\"strata_names\" should give one or more variable names in target data") 
  if (!identical(length(ctrl@strata_names), length(unique(ctrl@strata_names))))
    stop("\"strata_names\" should give unique variable names")
  else 
    TRUE
}

check_aggregates <- function(aggregate_data, ctrl) {
  nm <- names(aggregate_data)
  if (!length(aggregate_data))
    return (TRUE)
  if (!all(dim(aggregate_data) > 0))
    stop("not all dimensions of aggregate data are positive")
  if (!ctrl@time_name %in% nm)
    stop("\"time_name\" should give a variable name in aggregate data")
  if (!ctrl@geo_name %in% nm)
    stop("\"geo_name\" should give a variable name in aggregate data")
  if (!all(ctrl@group_names %in% nm))
    stop("\"group_names\" should give one or more variable names in aggregate_data")
  if (!"item" %in% nm)
    stop("\"item\" should be a variable name in aggregate_data")
  if (!"n_grp" %in% nm)
    stop("\"n_grp\" should be a variable name in aggregate_data")
  if (!"s_grp" %in% nm)
    stop("\"s_grp\" should be a variable name in aggregate_data")
}

check_modifiers <- function(modifier_data, ctrl) {
  nm <- names(modifier_data)
  if (!length(modifier_data))
    return(TRUE)
  if (!all(dim(modifier_data) > 0))
    stop("not all dimensions of hierarchical data are positive")
  if (!ctrl@time_name %chin% nm)
    stop("\"time_name\" should give a variable name in modifier data")
  if (!ctrl@geo_name %chin% nm)
    stop("\"geo_name\" should give a variable name in modifier data")
  if (!(length(ctrl@modifier_names) > 0L && all(ctrl@modifier_names %chin% nm)))
    stop("\"modifier_names\" should give one or more variable names in hierarchical data") 
  if (!(length(ctrl@t1_modifier_names) > 0L && all(ctrl@t1_modifier_names %chin% nm)))
    stop("\"t1_modifier_names\" should give one or more variable names in hierarchical data") 
  else
    TRUE
}

check_item <- function(item_data, ctrl) {
  nm <- names(item_data)
  if (length(ctrl@time_name) != 1L || !ctrl@time_name %chin% nm)
    stop("\"time_name\" should give a single variable name in item data")
  if (length(ctrl@geo_name) != 1L || !ctrl@geo_name %chin% nm)
    stop("\"geo_name\" should give a single variable name in item data")

  if (length(ctrl@survey_name) != 1L || !ctrl@survey_name %chin% nm)
    stop("\"survey_name\" should give a single variable name in item data")
  if (length(ctrl@weight_name) != 1L || !ctrl@weight_name %chin% nm)
    stop("\"weight_name\" should give a single variable name in item data")

  if (!(length(unique(ctrl@group_names)) > 0L) && all(ctrl@group_names %chin% nm))
    stop("\"group_names\" should give one or more variable names in item data")

  if (!(length(unique(ctrl@item_names)) > 0L) && all(ctrl@item_names %chin% nm))
    stop("\"item_names\" should give one or more variable names in item_names data")

  else
    TRUE
}
