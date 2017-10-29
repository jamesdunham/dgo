#' A class for data ready to model
#'
#' \code{shape()} generates objects of class \code{dgirtIn} for modeling with
#' \code{dgirt()} and \code{dgmrp()}.
#'
#' @aliases dgirtin-class, get_item_n, get_item_names, get_n, dgirtIn-method,
#'   print.dgirtIn,
#' @name dgirtin-class
#' @include constants.r
#' @import R6
#' @examples
#' data(toy_dgirt_in)
#' get_item_names(toy_dgirt_in)
#' get_n(toy_dgirt_in)
#' get_n(toy_dgirt_in, by = "year")
#' get_n(toy_dgirt_in, by = "source")
#' get_item_n(toy_dgirt_in)
#' get_item_n(toy_dgirt_in, by = "year")
NULL

setOldClass("dgirtIn", "R6")
dgirtIn <- R6::R6Class("dgirtIn",
  public = c(
    # model objects (N, G, T, ...) and shape objects (item_data, etc.) are
    # public and initially NULL
    setNames(lapply(c(model_objects, shape_objects), function(x) NULL),
      c(model_objects, shape_objects)),
    # the class is instantiated from a Control object 
    initialize = function(ctrl) {
      if (length(ctrl@constant_item)) {
        self$constant_item <- ctrl@constant_item
      }
      self$mod_par_names <- c(ctrl@geo_name, ctrl@time_name)
      self$unmod_par_names <- ctrl@group_names
    },
    # the as_list method extracts attributes used in modeling as expected by
    # rstan. its arguments will be passed from a dgirt or dgmrp call 
    as_list = function(separate_t, delta_tbar_prior_mean, delta_tbar_prior_sd,
      innov_sd_delta_scale, innov_sd_theta_scale, hierarchical_model) {

      # model_objects is a character vector of attribute names for rstan data  
      d_in_list <- Map(function(x) self[[x]], private$model_objects)

      # separate_t is a boolean in the stan code
      if (length(separate_t) != 1L || !is.logical(separate_t)) {
        stop("\"separate_t\" should be a single logical value")
      } 
      d_in_list$separate_t <- separate_t

      # hierarchical_model is also boolean in the stan code
      if (length(hierarchical_model) != 1L || !is.logical(hierarchical_model)) {
        stop("\"hierarchical_model\" should be a single logical value")
      } 
      d_in_list$hierarchical_model <- hierarchical_model

      if (length(delta_tbar_prior_mean) != 1L || !is.numeric(delta_tbar_prior_mean)) {
        stop("\"delta_tbar_prior_mean\" should be a single real value")
      } 
      d_in_list$delta_tbar_prior_mean <- delta_tbar_prior_mean

      if (length(delta_tbar_prior_sd) != 1L || !is.numeric(delta_tbar_prior_sd)
        || delta_tbar_prior_sd < 0)
      {
        stop("\"delta_tbar_prior_sd\" should be a single positive real value")
      } 
      d_in_list$delta_tbar_prior_sd <- delta_tbar_prior_sd

      if (length(innov_sd_delta_scale) != 1L ||
        !is.numeric(innov_sd_delta_scale) || innov_sd_delta_scale < 0) {
        stop("\"innov_sd_delta_scale\" should be a single real value")
      } 
      d_in_list$innov_sd_delta_scale <- innov_sd_delta_scale

      if (length(innov_sd_theta_scale ) != 1L ||
        !is.numeric(innov_sd_theta_scale) || innov_sd_theta_scale < 0) {
        stop("\"innov_sd_theta_scale\" should be a single positive real value")
      } 
      d_in_list$innov_sd_theta_scale <- innov_sd_theta_scale

      d_in_list
    }),
  # keep track of which items will be used in modeling 
  private = list(model_objects = model_objects, shape_objects = shape_objects))
