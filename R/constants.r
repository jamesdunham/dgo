model_objects <- c("NNl2", "SSl2", "XX", "ZZ", "ZZ_prior", "G", "Q", "T", "N",
                   "P", "S", "H", "D", "Hprior", "WT", "l2_only", "G_hier",
                   "constant_item", "n_vec", "s_vec", "observed", "N_observed")

shape_objects <- c("gt_items", "group_grid", "group_grid_t", "group_counts",
                  "item_data", "target_data", "aggregate_data", "modifier_data",
                  "control", "hier_names", "time_observed", "geo_observed",
                  "call", "mod_par_names", "unmod_par_names", "pkg_version")

dgirt_pars <- c("separate_t", "delta_tbar_prior_mean", "delta_tbar_prior_sd",
                "innov_sd_delta_scale", "innov_sd_theta_scale", "version",
                 "hierarchical_model")

dgmrp_pars <- c("separate_t", "delta_tbar_prior_mean", "delta_tbar_prior_sd",
                "innov_sd_delta_scale", "innov_sd_theta_scale", "version")

default_pars <- c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
                  "nu_geo", "nu_geo_prior", "kappa", "sd_item", "sd_theta",
                  "sd_theta_bar", "sd_gamma_geo", "sd_gamma_demo",
                  "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd",
                  "sd_total")

default_pars_mrp <- c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
                  "nu_geo", "nu_geo_prior", "sd_theta_bar", "sd_gamma_geo", "sd_gamma_demo",
                  "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd")

var_types <- list(group_names = c("character", "factor"),
                  geo_name = c("character", "factor"),
                  time_name = c("integer", "numeric"),
                  survey_name = c("character", "factor"),
                  weight_name = "numeric",
                  strata_names = c("character", "factor", "integer", "numeric"),
                  target_proportion_name = "numeric",
                  modifier_names = c("integer", "numeric"),
                  t1_modifier_names = c("integer", "numeric"),
                  item = c("character", "factor"),
                  n_grp = c("integer", "numeric"),
                  s_grp = c("integer", "numeric"))

index_names <- list("delta_gamma" = "time_name",
                     "delta_tbar" = "time_name",
                     # "diff" = c("unnamed_index", "unnamed_index"),
                     # "disc" = "unnamed_index",
                     "gamma" = c("time_name", "hier_params"),
                     "gamma_raw" = c("time_name", "hier_params"),
                     # "kappa" = c("unnamed_index" = "unnamed_index"),
                     # "mu_theta_bar" = c("unnamed_index", "unnamed_index"),
                     # "mu_gamma" = c("unnamed_index", "unnamed_index"),
                     "nu_geo" = c("time_name", NULL),
                     # "nu_geo_prior" = "unnamed_index",
                     # "sd_gamma" = NA,
                     # "sd_innov_delta" = NA,
                     # "sd_innov_gamma" = NA,
                     # "sd_innov_logsd" = NA,
                     "sd_item" = "item_names", 
                     "sd_theta" = "time_name",
                     "sd_theta_bar" = "time_name",
                     "sd_total" = "time_name",
                     "theta_l2" = c("time_name", NULL),
                     "theta_bar" = c("time_name", "group_names"),
                     "theta_bar_raw" = c("time_name", "group_names"),
                     "var_item" = "item_names",
                     "var_theta" = "time_name",
                     "var_theta_bar_l2" = c("time_name", NULL),
                     "xi" = "time_name",
                     "z" = c("time_name", NULL, "group_names"),
                     "z_l2" = c("time_name", NULL, NULL))
