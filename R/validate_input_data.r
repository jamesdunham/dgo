check_targets <- function(target_data, control) {
  if (!length(target_data))
    return(TRUE)
  if (!all(dim(target_data) > 0))
    stop("not all dimensions of target data are positive")
  if (!control@time_name %chin% names(target_data))
    stop("\"time_name\" should give a variable name in target data")
  if (!control@geo_name %chin% names(target_data))
    stop("\"geo_name\" should give a variable name in target data")
  if (!all(control@group_names %chin% names(target_data)))
    stop("\"group_names\" should give one or more variable names in target data")
  if (length(control@target_proportion_name) != 1 || !control@target_proportion_name %chin% names(target_data))
    stop("\"target_proportion_name\" should give a single variable name in target data")
  if (!length(control@strata_names) > 0L || !all(control@strata_names %chin% names(target_data)))
    stop("\"strata_names\" should give one or more variable names in target data") 
  if (!identical(length(control@strata_names), length(unique(control@strata_names))))
    stop("\"strata_names\" should give unique variable names")
  else 
    TRUE
}

check_modifiers <- function(modifier_data, control) {
  if (!length(modifier_data))
    return(TRUE)
  if (!all(dim(modifier_data) > 0))
    stop("not all dimensions of hierarchical data are positive")
  if (!control@time_name %chin% names(modifier_data))
    stop("\"time_name\" should give a variable name in modifier data")
  if (!control@geo_name %chin% names(modifier_data))
    stop("\"geo_name\" should give a variable name in modifier data")
  if (!(length(control@modifier_names) > 0L && all(control@modifier_names %chin% names(modifier_data))))
    stop("\"modifier_names\" should give one or more variable names in hierarchical data") 
  if (!(length(control@t1_modifier_names) > 0L && all(control@t1_modifier_names %chin% names(modifier_data))))
    stop("\"t1_modifier_names\" should give one or more variable names in hierarchical data") 
  else
    TRUE
}

check_item <- function(item_data, control) {
  if (length(control@time_name) != 1L || !control@time_name %chin% names(item_data))
    stop("\"time_name\" should give a single variable name in item data")
  if (length(control@geo_name) != 1L || !control@geo_name %chin% names(item_data))
    stop("\"geo_name\" should give a single variable name in item data")

  if (length(control@survey_name) != 1L || !control@survey_name %chin% names(item_data))
    stop("\"survey_name\" should give a single variable name in item data")
  if (length(control@weight_name) != 1L || !control@weight_name %chin% names(item_data))
    stop("\"weight_name\" should give a single variable name in item data")

  if (!(length(unique(control@group_names)) > 0L) && all(control@group_names %chin% names(item_data)))
    stop("\"group_names\" should give one or more variable names in item data")

  if (!(length(unique(control@item_names)) > 0L) && all(control@item_names %chin% names(item_data)))
    stop("\"item_names\" should give one or more variable names in item_names data")

  else
    TRUE
}
