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
                         ...) {
  ctrl <- new("Control", item_names = item_names,
                 time_name = time_name, geo_name = geo_name, group_names =
                   group_names, weight_name = weight_name, survey_name =
                   survey_name, ...)

  is_name <- valid_names(item_data, ctrl, 1L)
  is_name(c("time_name", "geo_name"))
  has_type(c("time_name", "geo_name"), item_data, ctrl)
  if (!length(ctrl@time_filter)) {
    ctrl@time_filter <- sort(unique(item_data[[ctrl@time_name]]))
  }
  if (!length(ctrl@geo_filter)) {
    ctrl@geo_filter <- sort(unique(as.character(item_data[[ctrl@geo_name]])))
  }
  ctrl
}

setValidity("Control",
            function(object) {
              if (!length(object@time_name) == 1L)
                "\"time_name\" should be a single variable name"
              if (!length(object@geo_name) == 1L)
                "\"geo_name\" should be a single variable name"
              if (!length(object@survey_name) == 1L)
                "\"survey_name\" should be a single variable name"
              if (!length(object@weight_name) == 1L)
                "\"weight_name\" should be a single variable name"
              if (!length(unique(object@group_names)) > 1L)
                "\"group_names\" should be at least one variable name" 
              if (!length(object@constant_item) == 1L && is.logical(object@constant_item))
                "\"constant_item\" should be a single logical value"
              if (!length(unique(object@time_filter)) > 1L)
                "\"time_filter\" filter should indicate at least two periods or be NULL to include all observed periods"
              if (!length(unique(object@geo_filter)) > 1L)
                "\"geo_filter\" should indicate at least two units or be NULL to include all observed units"
              if (length(object@min_survey_filter) != 1L || object@min_survey_filter <= 0L)
                "\"min_survey_filter\" should be a positive integer"
              if (!length(object@min_t_filter) == 1L && object@min_t_filter > 0L)
                "\"min_t_filter\" should be a positive integer"
              else 
                TRUE
            })
