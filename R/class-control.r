setClass("Control",
         slots = list(constant_item = "logical",
                      geo_name = "character",
                      geo_filter = "character",
                      group_names = "character",
                      item_names = "character",
                      aggregate_item_names = "character",
                      min_survey_filter = "numeric",
                      min_t_filter = "numeric",
                      modifier_names = "character",
                      prop_name = "character",
                      standardize = "logical",
                      strata_names = "character",
                      survey_name = "character",
                      target_group_names = "character",
                      target_proportion_name = "character",
                      t1_modifier_names = "character",
                      time_name = "character",
                      time_filter = "numeric",
                      weight_name = "character"),
         prototype = prototype(constant_item = TRUE,
                               min_t_filter = 1L,
                               min_survey_filter = 1L,
                               standardize = FALSE,
                               prop_name = "proportion"),
         validity = function(object) {
           if (!length(object@time_name) == 1L)
             "\"time_name\" should be a single variable name"
           else if (!length(object@geo_name) == 1L)
             "\"geo_name\" should be a single variable name"
           else if (!length(object@survey_name) == 1L)
             "\"survey_name\" should be a single variable name"
           else if (!length(object@standardize) == 1L)
             "\"standardize\" should be a single logical"
           else if (!length(object@weight_name) == 1L)
             "\"weight_name\" should be a single variable name"
           else if (!length(unique(object@group_names)) > 0L)
             "\"group_names\" should be at least one variable name" 
           else if (!length(object@constant_item) == 1L &&
                    is.logical(object@constant_item))
             "\"constant_item\" should be a single logical value"
           else if (length(unique(object@time_filter)) == 1L)
             "if specified \"time_filter\" should give at least two time periods"
           else if (length(unique(object@geo_filter)) == 1L)
             "if specified \"geo_filter\" should give at least two local geographic areas"
           else if (length(object@min_survey_filter) != 1L || object@min_survey_filter <= 0L)
             "\"min_survey_filter\" should be a positive integer"
           else if (!length(object@min_t_filter) == 1L &&
                    object@min_t_filter > 0L)
             "\"min_t_filter\" should be a positive integer"
           else 
             TRUE
         })
