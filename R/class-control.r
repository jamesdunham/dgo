setClass("Control",
         slots = list(constant_item = "logical",
                      geo_name = "character",
                      geo_filter = "character",
                      group_names = "ANY",
                      item_names = "character",
                      id_vars = "ANY",
                      aggregate_item_names = "ANY",
                      min_survey_filter = "numeric",
                      min_t_filter = "numeric",
                      modifier_names = "character",
                      prop_name = "character",
                      raking = "ANY",
                      rake_names = "character",
                      standardize = "logical",
                      sample_margins = "list",
                      survey_name = "ANY",
                      target_group_names = "character",
                      target_proportion_name = "character",
                      t1_modifier_names = "character",
                      time_name = "character",
                      time_filter = "numeric",
                      weight_name = "ANY"),
         prototype = prototype(constant_item = TRUE,
                               min_t_filter = 1L,
                               min_survey_filter = 1L,
                               standardize = TRUE,
                               prop_name = "proportion"),
         validity = function(object) {
           if (!length(object@time_name) == 1L)
             "\"time_name\" should be a single variable name"
           else if (!length(object@geo_name) == 1L)
             "\"geo_name\" should be a single variable name"
           else if (length(object@survey_name) && length(object@survey_name) != 1L)
             "if specified \"survey_name\" should be a single variable name"
           else if (length(object@survey_name) && !is.character(object@survey_name))
             "if specified \"survey_name\" should be a single variable name"
           else if (length(object@group_names) && !is.character(object@group_names))
             "if specified \"group_names\" should give variable names in a character vector"
           else if (length(object@id_vars) && !is.character(object@id_vars))
             "if specified \"id_vars\" should give variable names in a character vector"
           else if (length(object@aggregate_item_names) && !is.character(object@aggregate_item_names))
             "if specified \"aggregate_item_names\" should give values in an \"item\" column of aggregate_data"
           else if (!length(object@standardize) == 1L)
             "\"standardize\" should be a single logical"
           else if (length(object@weight_name) && !is.character(object@weight_name))
             "if specified \"weight_name\" should be a single variable name"
           else if (length(object@weight_name) > 1)
             "if specified \"weight_name\" should be a single variable name"
           else if (length(object@raking) && !is.list(object@raking) &
                    !"formula" %in% class(object@raking))
             "\"raking\" should be a formula or a list of formulas"
           else if (length(object@raking) && is.list(object@raking) &&
                    !all(sapply(object@raking, class) %in% "formula"))
             "\"raking\" should be a formula or a list of formulas"
           else if (!length(object@constant_item) == 1L &&
                    is.logical(object@constant_item))
             "\"constant_item\" should be a single logical value"
          # else if (length(unique(object@time_filter)) == 1L)
          #   "if specified \"time_filter\" should give at least two time periods"
           else if (length(unique(object@geo_filter)) == 1L)
             "if specified \"geo_filter\" should give at least two local geographic areas"
           else if (length(object@min_survey_filter) != 1L || object@min_survey_filter <= 0L)
             "\"min_survey_filter\" should be a positive integer"
           else if (!length(object@min_t_filter) == 1L && object@min_t_filter > 0L)
             "\"min_t_filter\" should be a positive integer"
           else if (length(object@sample_margins) && !is.list(object@sample_margins))
             "\"sample_margins\" should be a list of formulas or data frames"
           else 
             TRUE
         })
