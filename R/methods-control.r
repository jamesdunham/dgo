`:=` <- data.table::`:=`

# Constructor for Control
init_control <- function(item_data,
                         modifier_data,
                         target_data,
                         aggregate_data,
                         ...) {
  control <- new("Control", ...)
  # use item_data to set defaults for time_filter and geo_filter
  if (!length(control@time_name) || !control@time_name %in% names(item_data)) {
    stop("`time_name` (" , control@time_name,  ") is not a name in `item_data`")
  } else if (!length(control@time_filter)) {
    control@time_filter <- sort(unique(item_data[[control@time_name]]))
  }
  if (!length(control@geo_name) || !control@geo_name %in% names(item_data)) {
    stop("`geo_name` (" , control@geo_name,  ") is not a name in `item_data`")
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
              if (!length(object@separate_t) == 1L)
                "\"separate_t\" should be a single logical value"
              if (!length(object@constant_item) == 1L)
                "\"constant_item\" should be a single logical value"
              if (!length(object@delta_tbar_prior_mean) == 1L)
                "\"delta_tbar_prior_mean\" should be a single real value"
              if (!length(object@delta_tbar_prior_sd) == 1L)
                "\"delta_tbar_prior_sd\" should be a single positive real value"
              if (!length(object@innov_sd_delta_scale ) == 1L)
                "\"delta_tbar_delta_scale\" should be a single real value"
              if (!length(object@innov_sd_theta_scale ) == 1L)
                "\"delta_tbar_theta_scale\" should be a single real value"
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
