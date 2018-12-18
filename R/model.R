# A validation method for the Model class
check_model = function(object) {
  counts = c("T", "G", "G_hier", "Q", "D", "N", "P", "S", "H", "Hprior",
    "N_observed")
  errors = list(
    # Validate dimensions
    validate_that(are_equal(length(object@hier_names), dim(object@ZZ)[[2]])),
    validate_that(equal_length(object@n_vec, object@s_vec)),
    validate_that(all_equal(dim(object@NNl2), as.integer(c(object@T, object@Q, object@G_hier)))),
    validate_that(all_equal(dim(object@SSl2), as.integer(c(object@T, object@Q, object@G_hier)))),
    validate_that(all_equal(dim(object@WT), as.integer(c(object@T, object@G_hier, object@G)))),
    validate_that(all_equal(dim(object@l2_only), c(object@T, object@Q))),
    validate_that(all_equal(dim(object@XX), c(object@G, dim(object@ZZ)[2]))),
    validate_that(all_equal(dim(object@ZZ), c(object@T, object@P, object@H))),
    validate_that(all_equal(dim(object@ZZ_prior), c(object@T, object@P, object@Hprior))),
    validate_that(not_empty((object@constant_item))),
    validate_that(all_equal(object@N, object@G * object@T * object@Q)),
    validate_that(all_equal(dimnames(object@ZZ)[[2]], colnames(object@XX))),
    validate_that(all_equal(dimnames(object@ZZ_prior)[[2]], colnames(object@XX))),
    # Validate values
    validate_that(are_equal(sort(unique(as.vector(object@XX))), c(0L, 1L))),
    validate_that(all(object@WT >= 0)),
    # n_vec is a count
    validate_that(noNA(object@n_vec)),
    validate_that(all(object@n_vec %% 1 == 0)),
    validate_that(all(object@n_vec >= 0)),
    # s_vec is a count
    validate_that(noNA(object@s_vec)),
    validate_that(all(object@s_vec %% 1 == 0)),
    validate_that(all(object@s_vec >= 0)),
    # observed gives indexes of positive n_vec
    validate_that(noNA(object@observed)),
    validate_that(all(object@observed %% 1 == 0)),
    validate_that(all(object@observed >= 0)),
    # no more successes than trials
    validate_that(all(object@s_vec <= object@n_vec)),
    # each of these gives a parameter count
    lapply(counts, function(p) validate_that(is.count(slot(object, p)))),
    # The count of geographic predictors must be positive but no larger than the
    # count of geographic or demographic predictors.
    validate_that(object@S <= object@P),
    validate_that(object@S > 0),
    validate_that(ifelse(object@constant_item, identical(object@D, 1L),
        identical(object@D, object@T)))
    )
  errors = errors[!sapply(errors, isTRUE)]
  if (length(errors)) {
    return(errors)
  } else {
    return(TRUE)
  }
}

#' A class for Stan model data
#'
#' @slot D Number of difficulty parameters per question.
#' @slot G Number of covariate groups.
#' @slot G_hier Not implemented.
#' @slot H Number of predictors for geographic unit effects.
#' @slot Hprior Number of predictors for geographic unit effects in first time
#' period.
#' @slot N Number of covariate groups.
#' @slot NNl2 Not implemented.
#' @slot N_observed Number of covariate groups with at least one trial.
#' @slot P Number of hierarchical parameters, including geographic parameters.
#' @slot Q Number of items, after dichotomizing polytomous items.
#' @slot S Number of geographic parameters.
#' @slot SSl2 Not implemented.
#' @slot T Number of time periods.
#' @slot WT Weight array.
#' @slot XX Design matrix for hierarchical variables.
#' @slot ZZ Data for geographic model.
#' @slot ZZ_prior Data for geographic model in the first time period.
#' @slot constant_item Flag for whether item parameters are constant.
#' @slot delta_tbar_prior_mean Prior mean for \code{delta_tbar}.
#' @slot delta_tbar_prior_sd Prior standard deviation for \code{delta_tbar}.
#' @slot hierarchical_model Flag for whether to use the hierarchical model.
#' @slot innov_sd_delta_scale Scale parameter for innovation of \code{sd_delta}.
#' @slot innov_sd_theta_scale Scale parameter for innovation of \code{sd_theta}.
#' @slot l2_only Not implemented.
#' @slot n_vec Vector of covariate group trial counts.
#' @slot observed Non-missing indexes for \code{n_vec} and \code{s_vec}.
#' @slot s_vec Vector of covariate group success counts.
#' @slot separate_t Flag for whether to smooth estimates over time periods.
#' @aliases Model
setClass("Model",
  slots = list(
    D = "integer",
    G = "integer",
    G_hier = "integer",
    H = "integer",
    Hprior = "integer",
    N = "integer",
    NNl2 = "array",
    N_observed = "integer",
    P = "integer",
    Q = "integer",
    S = "integer",
    SSl2 = "array",
    T = "integer",
    WT = "array",
    XX = "matrix",
    ZZ = "array",
    ZZ_prior = "array",
    constant_item = "integer",
    delta_tbar_prior_mean = "numeric",
    delta_tbar_prior_sd = "numeric",
    hierarchical_model = "integer",
    innov_sd_delta_scale = "numeric", 
    innov_sd_theta_scale = "numeric", 
    l2_only = "matrix",
    # FIXME: should only be integer, but arrives as numeric
    n_vec = "numeric",
    observed = "integer",
    # FIXME: should only be integer, but arrives as numeric
    s_vec = "numeric",
    separate_t = "integer"),
  validity = check_model)

setMethod("initialize", "Model", 
  function(.Object, shaped, ...) {
    .Object@G = G(shaped)
    .Object@G_hier = G_hier(shaped, .Object@G)
    .Object@N = N(shaped)
    .Object@Q = Q(shaped)
    .Object@T = T(shaped)
    .Object@WT = array(1, dim = c(.Object@T, .Object@G_hier, .Object@G))
    .Object@XX = XX(shaped)
    .Object@ZZ = ZZ(shaped)
    .Object@ZZ_prior = ZZ(shaped, t1 = TRUE)
    .Object@H = dim(.Object@ZZ)[[3]]
    .Object@Hprior = dim(.Object@ZZ_prior)[[3]]
    .Object@P = ncol(.Object@ZZ)
    .Object@S = S(shaped)
    .Object@l2_only = matrix(0L, nrow = .Object@T, ncol = .Object@Q)
    .Object@n_vec = n_vec(shaped)
    .Object@observed = observed(shaped)
    .Object@N_observed = length(.Object@observed)
    .Object@s_vec = s_vec(shaped)
    .Object@NNl2 = array(0L, dim = c(.Object@T, .Object@Q, .Object@G_hier))
    .Object@SSl2 = .Object@NNl2
    # the remaining slots may be overridden by dgirt/dgmrp
    .Object@constant_item = 1L
    .Object@hierarchical_model = 1L
    .Object@separate_t = 0L
    # D should be 1L if constant_item is TRUE, otherwise equal to T 
    .Object@D = 1L  
    .Object@delta_tbar_prior_mean = 0.65
    .Object@delta_tbar_prior_sd = 0.25
    .Object@innov_sd_delta_scale = 2.5
    .Object@innov_sd_theta_scale = 2.5
    .Object
})

setMethod("as.list", "Model", function(x, ...) {
  nms <- slotNames(x)
  names(nms) <- nms
  lapply(nms, slot, object = x)
})

