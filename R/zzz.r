`:=` <- data.table::`:=`

setClass("Control",
         slots = list(constant_item = "logical",
                      delta_tbar_prior_mean = "numeric",
                      delta_tbar_prior_sd = "numeric",
                      geo_name = "character",
                      geo_filter = "character",
                      group_names = "character",
                      innov_sd_delta_scale = "numeric",
                      innov_sd_theta_scale = "numeric",
                      item_names = "character",
                      min_survey_filter = "numeric",
                      min_t_filter = "numeric",
                      modifier_names = "character",
                      prop_name = "character",
                      separate_t = "logical",
                      strata_names = "character",
                      survey_name = "character",
                      target_group_names = "character",
                      target_proportion_name = "character",
                      t1_modifier_names = "character",
                      time_name = "character",
                      time_filter = "numeric",
                      weight_name = "character"),
         prototype = prototype(separate_t = FALSE,
                               constant_item = TRUE,
                               delta_tbar_prior_mean = 0.5,
                               delta_tbar_prior_sd = 0.5,
                               innov_sd_theta_scale = 2.5,
                               innov_sd_delta_scale = 2.5,
                               min_t_filter = 1L,
                               min_survey_filter = 1L,
                               prop_name = "proportion"))

setValidity("Control",
            function(object) {
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


# TODO: create classes for each input object with individual validation, and then use dgirtInput validation for
# validiation across objects
setClass("dgirtIn",
         slots = list(NNl2 = "ANY",
                      SSl2 = "ANY",
                      XX = "ANY",
                      ZZ = "ANY",
                      ZZ_prior = "ANY",
                      MMM = "ANY",
                      G = "ANY",
                      Q = "ANY",
                      T = "ANY",
                      N = "ANY",
                      P = "ANY",
                      S = "ANY",
                      H = "ANY",
                      Hprior = "ANY",
                      WT = "ANY",
                      l2_only = "ANY",
                      Gl2 = "ANY",
                      group_counts = "ANY",
                      group_grid = "data.frame",
                      group_grid_t = "data.frame",
                      separate_t = "logical",
                      constant_item = "logical",
                      delta_tbar_prior_mean = "numeric",
                      delta_tbar_prior_sd = "numeric",
                      innov_sd_theta_scale = "numeric",
                      innov_sd_delta_scale = "numeric",
                      n_vec = "numeric",
                      s_vec = "numeric",
                      vars = "list"),
         prototype = prototype(separate_t = FALSE,
                               constant_item = TRUE,
                               delta_tbar_prior_mean = 0.5,
                               delta_tbar_prior_sd = 0.5,
                               innov_sd_theta_scale = 2.5,
                               innov_sd_delta_scale = 2.5))

setValidity("dgirtIn",
            function(object) {
              if (FALSE)
                "stop message"
              else
                TRUE
            })
