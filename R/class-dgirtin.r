#' Data for fitting a DGIRT Model.
#' @name dgirtin-class
#' @include constants.r
NULL

setOldClass("dgirtIn", "R6")
dgirtIn <- R6::R6Class("dgirtIn",
  public = c(
    setNames(lapply(c(model_objects, shape_objects), function(x) NULL),
             c(model_objects, shape_objects)),
    initialize = function(item_data,
                          modifier_data,
                          target_data,
                          aggregate_data,
                          control) {

      if (length(control@constant_item)) {
        self$constant_item <- control@constant_item
      }

      if (length(item_data)) {
        item_data_names <- c(control@item_names, control@group_names,
                             control@geo_name, control@time_name,
                             control@weight_name)
        if (!all(item_data_names %chin% names(item_data))) {
          stop("Not found as names in item data: ",
               paste(setdiff(item_data_names, names(item_data)), collapse = ", "))
        }
      }

      if (length(modifier_data)) {
        modifier_data_names <- c(control@modifier_names, control@t1_modifier_names,
                                 control@geo_name, control@time_name)
        if (!all(modifier_data_names %chin% names(modifier_data))) {
          stop("Not found as names in modifier data: ",
               paste(setdiff(modifier_data_names, names(item_data)), collapse = ", "))
        }
      }

      if (length(target_data)) {
        target_data_names <- c(control@target_proportion_name, control@strata_names,
                               control@geo_name, control@time_name)
        if (!all(target_data_names %chin% names(target_data))) {
          stop("Not found as names in target data: ",
               paste(setdiff(target_data_names, names(item_data)), collapse = ", "))
        }
      }

      if (length(aggregate_data)) {
        aggregate_data_names <- c(control@geo_name, control@time_name, "item",
                                  "n_grp", "s_grp")
        if (!all(aggregate_data_names %chin% names(aggregate_data))) {
          stop("Not found as names in aggregate data: ",
               paste(setdiff(aggregate_data_names, names(item_data)), collapse = ", "))
        }
      }
    },
    as_list = function(separate_t, delta_tbar_prior_mean, delta_tbar_prior_sd,
                       innov_sd_delta_scale, innov_sd_theta_scale) {
      d_in_list <- Map(function(x) self[[x]], private$model_objects)
      # if called from `dgirt` look for modeling-choice arguments passed in ...
      if (!length(separate_t) == 1L && is.logical(separate_t))
        stop("\"separate_t\" should be a single logical value")
      else d_in_list$separate_t <- separate_t
      if (!length(delta_tbar_prior_mean) == 1L &&
          is.numeric(delta_tbar_prior_mean))
        stop("\"delta_tbar_prior_mean\" should be a single real value")
      else d_in_list$delta_tbar_prior_mean <- delta_tbar_prior_mean 
      if (!length(delta_tbar_prior_sd) == 1L && is.numeric(delta_tbar_prior_sd))
        stop("\"delta_tbar_prior_sd\" should be a single positive real value")
      else d_in_list$delta_tbar_prior_sd <- delta_tbar_prior_sd 
      if (!length(innov_sd_delta_scale ) == 1L && is.numeric(innov_sd_delta_scale))
        stop("\"delta_tbar_delta_scale\" should be a single real value")
      else d_in_list$innov_sd_delta_scale <- innov_sd_delta_scale 
      if (!length(innov_sd_theta_scale ) == 1L && is.numeric(innov_sd_theta_scale))
        stop("\"delta_tbar_theta_scale\" should be a single real value")
      else d_in_list$innov_sd_theta_scale <- innov_sd_theta_scale 
      d_in_list
    }),
  private = list(model_objects = model_objects,
                 shape_objects = shape_objects))
