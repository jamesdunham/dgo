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
  control <- new("Control", item_names = item_names,
                 time_name = time_name, geo_name = geo_name, group_names =
                   group_names, weight_name = weight_name, survey_name =
                   survey_name, ...)
  # use item_data to set defaults for time_filter and geo_filter
  if (!length(control@time_name) || !control@time_name %in% names(item_data)) {
    stop("`time_name` (" , control@time_name,  ") should be a name in `item_data`")
  } else if (!length(control@time_filter)) {
    control@time_filter <- sort(unique(item_data[[control@time_name]]))
  }
  if (!length(control@geo_name) || !control@geo_name %in% names(item_data)) {
    stop("`geo_name` (" , control@geo_name,  ") should be a name in `item_data`")
  } else if (!length(control@geo_filter)) {
    control@geo_filter <- sort(unique(as.character(item_data[[control@geo_name]])))
  }
  control
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
