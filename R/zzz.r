`:=` <- data.table::`:=`

# Constructor for Control
control <- function(group_names,
                    item_names,
                    geo_name,
                    time_name,
                    weight_name,
                    ...) {

  mget(names(formals(wrangle)), parent.frame(),
    ifnotfound = list(rep(NULL, length(formals(wrangle)))))

  dots <- list(...)
  if (!length(dots$time_filter))
    dots$time_filter <- 
  if (!length(dots$geo_filter))
    dots$geo_filter <- 
  new("Control", group_names = group_names, item_names = item_names,
      geo_name = geo_name, time_name = time_name, weight_name = weight_name,
      dots)
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
