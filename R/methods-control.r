`:=` <- data.table::`:=`
`%chin%` <- data.table::`%chin%`

# Constructor for Control
init_control <- function(item_data,
                         item_names,
                         time_name,
                         geo_name,
                         group_names,
                         weight_name,
                         survey_name,
                         raking,
                         id_vars,
                         ...) {
  ctrl <- new("Control", item_names = item_names,
                 time_name = time_name, geo_name = geo_name, group_names =
                   group_names, weight_name = weight_name, survey_name =
                   survey_name, raking = raking, id_vars = id_vars, ...)

  is_name <- valid_names(item_data, ctrl, 1L)
  is_name(c("time_name", "geo_name"))
  has_type(c("time_name", "geo_name"), item_data, ctrl)
  if (!length(ctrl@time_filter)) {
    ctrl@time_filter <- sort(unique(item_data[[ctrl@time_name]]))
  }
  if (!length(ctrl@geo_filter)) {
    ctrl@geo_filter <- sort(unique(as.character(item_data[[ctrl@geo_name]])))
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
