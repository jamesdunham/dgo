model_objects <- c("NNl2", "SSl2", "XX", "ZZ", "ZZ_prior", "G", "Q", "T", "N",
                   "P", "S", "H", "D", "Hprior", "WT", "l2_only", "G_hier",
                   "constant_item", "n_vec", "s_vec", "observed", "N_observed")

shape_objects <- c("gt_items", "group_grid", "group_grid_t", "group_counts",
                  "item_data", "target_data", "aggregate_data", "modifier_data",
                  "control", "hier_names", "time_observed", "geo_observed",
                  "call")

dgirt_pars <- c("separate_t", "delta_tbar_prior_mean", "delta_tbar_prior_sd",
                "innov_sd_delta_scale", "innov_sd_theta_scale")

var_types <- list(item_names = c("integer", "numeric"),
                  group_names = c("character", "factor"),
                  geo_name = c("character", "factor"),
                  time_name = c("integer", "numeric"),
                  survey_name = c("character", "factor"),
                  weight_name = "numeric",
                  strata_names = c("character", "factor"),
                  target_proportion_name = "numeric",
                  modifier_names = c("integer", "numeric"),
                  t1_modifier_names = c("integer", "numeric"),
                  item = c("character", "factor"),
                  n_grp = c("integer", "numeric"),
                  s_grp = c("integer", "numeric"))
