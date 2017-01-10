#' \code{dgmrp}: fit a single-issue MRP model, with hierarchical covariates
#'
#' \code{dgmrp} makes a call to \code{\link[rstan]{stan}} with the Stan code and
#' data for a dgmrp model.
#'
#' The user will typically pass further arguments to \code{\link[rstan]{stan}}
#' via the \code{...} argument, at a minimum \code{iter} and \code{cores}.
#'
#' By default \code{dgmrp} overrides the \code{\link[rstan]{stan}} default for its
#' \code{pars} argument to specify typical DGIRT parameters of interest.
#' \code{dgmrp} also sets \code{iter_r} to \code{1L}.
#'
#' @inheritParams dgirt
#' @return A \code{\link{dgirtfit-class}} object that extends
#' \code{\link[rstan]{stanfit-class}}.
#' @seealso \code{dgmrp} expects \code{shaped_data} created by
#'   \code{\link{shape}} and returns an object of class
#'   \code{\link{dgirtfit-class}}.
#' \code{\link{dgirtfit-class}} \code{\link{shape}}
#'
#' @import rstan
#' @export
#' @include constants.r
dgmrp <- function(shaped_data, ..., separate_t = FALSE, delta_tbar_prior_mean = 0.65,
                  delta_tbar_prior_sd = 0.25, innov_sd_delta_scale = 2.5,
                  innov_sd_theta_scale = 2.5, version = "2017_01_04_singleissue") {

  dots <- list(...,
               object = stanmodels[[version]],
               data = shaped_data$as_list(separate_t = separate_t,
                                          delta_tbar_prior_mean =
                                            delta_tbar_prior_mean,
                                          delta_tbar_prior_sd =
                                            delta_tbar_prior_sd,
                                          innov_sd_delta_scale =
                                            innov_sd_delta_scale,
                                          innov_sd_theta_scale =
                                            innov_sd_theta_scale))

  if (length(shaped_data$gt_items) > 1) {
  	  stop("Multiple items in item data. Single-issue MRP models can only include one item.")
  }
    
  if (!length(dots$pars)) {
    dots$pars <- default_pars_mrp
  }
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }
  dots <- dots[!names(dots) %in% dgirt_pars]

  stanfit <- do.call(rstan::sampling, dots)

  tryCatch(new("dgirtfit", stanfit, dgirt_in = shaped_data, call = match.call()),
           error = function(e) {
             warning("Error constructing dgirtfit; returning stanfit object instead")
             stanfit
           })
}

models <- function() {
  names(stanmodels)
}
