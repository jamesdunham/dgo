dgirtIn <- R6::R6Class("dgirtIn",
  public = c(
    setNames(lapply(c(model_objects, shape_objects), function(x) NULL),
             c(model_objects, shape_objects)),
    initialize = function(control) {
      self$constant_item <- control@constant_item
      self$delta_tbar_prior_mean <- control@delta_tbar_prior_mean
      self$delta_tbar_prior_sd <- control@delta_tbar_prior_sd
      self$innov_sd_delta_scale <- control@innov_sd_delta_scale
      self$innov_sd_theta_scale <- control@innov_sd_theta_scale
      self$separate_t <- control@separate_t
    },
    as_list = function() {
      Map(function(x) self[[x]], private$model_objects)
    }),
  private = list(model_objects = model_objects,
                 shape_objects = shape_objects))
