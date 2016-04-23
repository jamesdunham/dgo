#' Data for fitting a DGIRT Model.
#' @name dgirtin-class
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
      self$constant_item <- control@constant_item
      self$delta_tbar_prior_mean <- control@delta_tbar_prior_mean
      self$delta_tbar_prior_sd <- control@delta_tbar_prior_sd
      self$innov_sd_delta_scale <- control@innov_sd_delta_scale
      self$innov_sd_theta_scale <- control@innov_sd_theta_scale
      self$separate_t <- control@separate_t

      if (length(item_data)) {
        item_data_names <- c(control@item_names, control@group_names,
                             control@geo_name, control@time_name,
                             control@weight_name)
        if (!all(item_data_names %chin% names(item_data))) {
          stop("Not found as names in item data: ",
               setdiff(item_data_names, names(item_data)))
        }
      }

      if (length(modifier_data)) {
        modifier_data_names <- c(control@modifier_names, control@t1_modifier_names,
                                 control@geo_name, control@time_name)
        if (!all(modifier_data_names %chin% names(modifier_data))) {
          stop("Not found as names in modifier data: ",
               setdiff(modifier_data_names, names(item_data)))
        }
      }

      if (length(target_data)) {
        target_data_names <- c(control@target_proportion_name, control@strata_names,
                               control@geo_name, control@time_name)
        if (!all(target_data_names %chin% names(target_data))) {
          stop("Not found as names in target data: ",
               setdiff(target_data_names, names(item_data)))
        }
      }

      if (length(aggregate_data)) {
        aggregate_data_names <- c(control@geo_name, control@time_name, "item",
                                  "n_grp", "s_grp")
        if (!all(aggregate_data_names %chin% names(aggregate_data))) {
          stop("Not found as names in aggregate data: ",
               setdiff(aggregate_data_names, names(item_data)))
        }
      }
    },
    as_list = function() {
      Map(function(x) self[[x]], private$model_objects)
    }),
  private = list(model_objects = model_objects,
                 shape_objects = shape_objects))
