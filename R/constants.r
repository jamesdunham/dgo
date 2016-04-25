model_objects <- c("NNl2", "SSl2", "XX", "ZZ", "ZZ_prior", "MMM", "G", "Q", "T",
                  "N", "P", "S", "H", "D", "Hprior", "WT", "l2_only", "G_hier",
                  "constant_item", "n_vec", "s_vec")

shape_objects <- c("gt_items", "group_grid", "group_grid_t", "group_counts",
                  "item_data", "target_data", "aggregate_data", "modifier_data",
                  "control", "hier_names", "time_observed", "geo_observed",
                  "call")

# to be passed in the call to dgirt in ... but dropped from the call to stan
dgirt_pars <- c("separate_t", "delta_tbar_prior_mean", "delta_tbar_prior_sd",
                "innov_sd_delta_scale", "innov_sd_theta_scale")
