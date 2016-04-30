check_targets <- function(target_data, ctrl) {
  if (length(target_data)) {
    is_name <- valid_names(target_data, ctrl, 1L)
    are_names <- valid_names(target_data, ctrl)
    is_name(c("time_name", "geo_name", "target_proportion_name"))
    are_names(c("strata_names", "group_names"))
  }
}

check_aggregates <- function(aggregate_data, ctrl) {
  if (length(aggregate_data)) {
    is_name <- valid_names(aggregate_data, ctrl, 1L)
    are_names <- valid_names(aggregate_data, ctrl)
    each_is_name <- valid_names(aggregate_data)
    is_name(c("time_name", "geo_name"))
    are_names("group_names")
    each_is_name(c("item", "n_grp", "s_grp"))
    if (!length(ctrl@aggregate_item_names)) {
      stop("argument \"aggregate_item_names\" is missing, with no default")
    }
    if (!all(ctrl@aggregate_item_names %chin% aggregate_data[["item"]])) {
      stop("\"aggregate_item_name\" should give values of \"item\" variable in ",
           "\"aggregate_data")
    }
  }
}

check_modifiers <- function(modifier_data, ctrl) {
  if (length(modifier_data)) {
    is_name <- valid_names(modifier_data, ctrl, 1L)
    are_names <- valid_names(modifier_data, ctrl)
    is_name(c("time_name", "geo_name"))
    are_names(c("modifier_names", "t1_modifier_names"))
  }
}

check_item <- function(item_data, ctrl) {
  is_name <- valid_names(item_data, ctrl, 1L)
  are_names <- valid_names(item_data, ctrl)
  is_name(c("time_name", "geo_name", "survey_name", "weight_name"))
  are_names(c("group_names","item_names"))
}

valid_names <- function(where, s_four = NULL, len = 0L) {
  tab_name <- deparse(substitute(where))
  if (!all(dim(where)) > 0) {
    stop("all dimensions of ", tab_name, " should be positive")
  }
  function(all_v) {
    for (v in all_v) {
      if (eval(length(s_four))) {
        val <- slot(s_four, v)
      } else {
        val <- eval(v)
      }
      v_name <- deparse(substitute(v))
      if (!length(val)) {
        stop(v_name, " is required when using ", deparse(tab_name))
      } 
      if (len > 0 && length(val) != len) {
        stop(v_name, " should be length ", len, ", not ", length(val))
      }
      if (!all(val %in% names(where)) || any(val == "")) {
        stop(v_name, " should give",
             ngettext(len, " a variable name", " variable names"),
             " in ", deparse(tab_name))
      }
      if (!length(val) == length(unique(val)))
        stop(v_name, " should give unique names")
    }
  }
}
