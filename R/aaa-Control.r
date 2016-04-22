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
                      aggregate_item_names = "character",
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
