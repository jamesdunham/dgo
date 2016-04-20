`:=` <- data.table::`:=`

model_objects = c("NNl2", "SSl2", "XX", "ZZ", "ZZ_prior", "MMM", "G", "Q", "T", "N", "P", "S", "H", "D", "Hprior",
  "WT", "l2_only", "G_hier", "separate_t", "constant_item", "delta_tbar_prior_mean",
  "delta_tbar_prior_sd", "innov_sd_theta_scale", "innov_sd_delta_scale", "n_vec", "s_vec")

shape_objects = c("group_grid", "group_grid_t", "group_counts", "item_data", "target_data", "aggregate_data",
  "modifier_data", "control", "vars")

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

# TODO: add to doc
# # where ... can be: 
# time_filter
# geo_filter
#
# min_survey_filter
# min_t_filter
# survey_name
#
# constant_item
# delta_tbar_prior_mean
# delta_tbar_prior_sd
# innov_sd_delta_scale
# innov_sd_theta_scale
# separate_t
#
# modifier_names
# t1_modifier_names
#
# target_group_names
# target_proportion_name
# strata_names

# # Setup for new Control object
# control <- new("Control", 
#              constant_item = TRUE,
#              delta_tbar_prior_mean = 2.5,
#              delta_tbar_prior_sd = 0.5,
#              geo_filter = as.character(sort(unique(state_opinion$state))),
#              geo_name = "state",
#              group_names = "race",
#              innov_sd_delta_scale = 2.5,
#              innov_sd_theta_scale = 2.5,
#              item_names = "Q_cces2006_minimumwage",
#              min_survey_filter = 1L,
#              min_t_filter = 1L,
#              modifier_names = "income_percapita",
#              target_proportion_name = "proportion",
#              separate_t = TRUE,
#              strata_names = "race",
#              survey_name = "source",
#              t1_modifier_names = "income_percapita",
#              target_group_names = "race",
#              time_filter = 2006:2014,
#              time_name = "year",
#              weight_name = "weight")

# Constructor for Control
control <- function(group_names,
                    item_names,
                    geo_name,
                    time_name,
                    weight_name,
                    ...) {
  new("Control", group_names = group_names, item_names = item_names,
      geo_name = geo_name, time_name = time_name, weight_name = weight_name,
      ...)
}

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

# Extend stanfit-class
dgirtFit <- setClass("dgirtFit",
                     contains = "stanfit",
                     slots = list(dgirt_in = "ANY",
                                  stan_data = "list",
                                  dgirt_vars = "list",
                                  control = "Control"))

setMethod("show", "dgirtFit",
          function(object = dgirtFit) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object)
          })

setMethod("summary", "dgirtFit",
          function(object = dgirtFit, ...) {
            object@sim$fnames_oi <- flatnames(object)
            callNextMethod(object, ...)
          })

setMethod("extract", "dgirtFit",
          function(object = dgirtFit, ...) {
            dots <- list(...)
            extracted <- callNextMethod(object, ...)
            if (!length(dots$permuted) || dots$permuted)
              # permuted is TRUE so extract returns a list of arrays
              extracted <- arraynames(extracted, object)
            else
              # permuted = FALSE so extract returns an array with dimensions iterations x chains x parameters
              dimnames(extracted)[[3]] <- flatnames(object, dimnames(extracted)[[3]])
            extracted
          }) 

setMethod("get_posterior_mean", "dgirtFit",
          function(object = dgirtFit, ...) {
            posterior_means <- callNextMethod(object, ...)
            rownames(posterior_means) <- flatnames(object, rownames(posterior_means))
            posterior_means
          })
