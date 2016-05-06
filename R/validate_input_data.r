check_targets <- function(target_data, ctrl) {
  if (length(target_data)) {
    is_name <- valid_names(target_data, ctrl, 1L)
    are_names <- valid_names(target_data, ctrl)
    is_name(c("time_name", "geo_name", "target_proportion_name"))
    are_names(c("strata_names", "group_names"))
    has_type(c("time_name", "geo_name",
               "target_proportion_name", "strata_names",
               "group_names"), target_data, ctrl)
    check_time(target_data, ctrl@time_name) 
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
    has_type(c("time_name", "geo_name", "group_names"), aggregate_data, ctrl)
    has_type(c("item", "n_grp", "s_grp"), aggregate_data, ctrl = NULL)
    for (v in c("s_grp", "n_grp")) {
      check_count(aggregate_data, v)
    }
    if (!all(ctrl@aggregate_item_names %chin% aggregate_data[["item"]])) {
      stop("\"aggregate_item_name\" should give values of \"item\" variable in ",
           "aggregate_data")
    }
    check_time(aggregate_data, ctrl@time_name) 
  }
}

check_modifiers <- function(modifier_data, ctrl) {
  if (length(modifier_data)) {
    is_name <- valid_names(modifier_data, ctrl, 1L)
    are_names <- valid_names(modifier_data, ctrl)
    is_name(c("time_name", "geo_name"))
    are_names(c("modifier_names", "t1_modifier_names"))
    has_type(c("time_name", "geo_name", "modifier_names", "t1_modifier_names"),
             modifier_data, ctrl)
    sapply(unique(c(ctrl@modifier_names, ctrl@t1_modifier_names,
                    ctrl@time_name, ctrl@geo_name)), 
           function(x) {
             if (any(is.na(modifier_data[[x]])))
               stop("There are NA values in the ", deparse(x), " variable ",
                    "of the modifier data.")
           })
    check_time(modifier_data, ctrl@time_name) 
  }
}

check_item <- function(item_data, ctrl) {
  is_name <- valid_names(item_data, ctrl, 1L)
  are_names <- valid_names(item_data, ctrl)
  is_name(c("time_name", "geo_name", "survey_name", "weight_name"))
  are_names(c("group_names","item_names"))
  has_type(c("time_name", "geo_name", "group_names", "survey_name",
             "item_names", "weight_name"), item_data, ctrl)
  check_time(item_data, ctrl@time_name) 
}

stop_if_empty <- function(object) {
  if (!all(dim(object)) > 0) {
    stop("all dimensions of ", deparse(substitute(object)),
         " should be positive")
  }
}

has_type <- function(slots, where, ctrl, valid_types = var_types) {
  tab_name <- deparse(substitute(where))
  for (slot_name in slots) {
    if (length(ctrl)) {
      varnames <- slot(ctrl, slot_name)
    } else {
      varnames <- slot_name
    } 
    v_valid_types <- valid_types[[slot_name]]
    for (v in varnames) {
      v_class <- class(where[[v]])
      if (!any(v_valid_types %in% v_class)) {
        stop(slot_name, " \"", v, "\"", " is ", v_class, " in ", tab_name,
             " but should be ", cc_or(v_valid_types))
                                
      }
    }
  }
}

valid_names <- function(where, s_four = NULL, len = 0L, verb = "give") {
  stop_if_empty(where)
  tab_name <- deparse(substitute(where))
  function(all_v) {
    for (v in unlist(all_v)) {
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
        stop(v_name, " should ", verb, 
             ngettext(len, " a variable name", " variable names"),
             " in ", deparse(tab_name))
      }
      if (!length(val) == length(unique(val)))
        stop(v_name, " should give unique names")
    }
  }
}

check_count <- function(where, name) {
  if (any(where[[name]] %% 1 != 0L) || where[[name]] < 0) {
    stop("values of \"", name, "\" in ", deparse(substitute(where)),
         " should be positive integers")
  }
}

check_time <- function(where, name) {
  if (any(where[[name]] %% 1 != 0L)) {
    stop("values of time_name variable in ", deparse(substitute(where)), " (",
         name, ") should be integers for now")
  }
}

cc <- function(...) {
  s <- unlist(list(...))
  s <- trimws(s)
  paste(s, sep = ", ", collapse = ", ")
}

cc_or <- function(..., oxford = FALSE) {
  x = unlist(list(...))
  res <- cc(x[-length(x)])
  comma <- ifelse(isTRUE(oxford) && length(x) > 2, ",", "")
  or <- ifelse(length(x) > 1L, " or ", "")
  paste0(res, comma, or, x[length(x)])
}

cc_and <- function(..., oxford = FALSE) {
  x = unlist(list(...))
  res <- cc(x[-length(x)])
  comma <- ifelse(isTRUE(oxford) && length(x) > 2, ",", "")
  and <- ifelse(length(x) > 1L, " and ", "")
  paste0(res, comma, and, x[length(x)])
}

