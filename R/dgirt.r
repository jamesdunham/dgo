#' Estimate a DGIRT model
#'
#' `dgirt` makes a call to `\link[rstan]{stan}` with the Stan code and data for
#' a DGIRT model. By default the call overrides the `stan` defaults for `pars`
#' to specify typical DGIRT parameters of interest and sets `iter_r` to `1L`.
#'
#' Additional arguments giving modeling choices:
#'
#' \describe{
#'   \item{separate_t}{Whether smoothing of estimates over time should be
#'   disabled. Default `FALSE`.}
#'   \item{delta_tbar_prior_mean}{Prior mean for `delta_tbar`, the normal weight
#'   on `theta_bar` in the previous period.  Default `0.5`.}
#'   \item{delta_tbar_prior_sd}{Prior standard deviation for `delta_bar`.
#'   Default `0.5`.}
#'   \item{innov_sd_delta_scale}{Prior scale for `sd_innov_delta`, the Cauchy
#'   innovation standard deviation of `nu_geo` and `delta_gamma`. Default
#'   `2.5`.}
#'   \item{innov_sd_theta_scale}{Prior scale for `sd_innov_theta`, the Cauchy
#'   innovation standard deviation of `gamma`, `xi`, and if `constant_item` is
#'   `FALSE` the item difficulty `diff`. Default `2.5`.}
#' }
#'
#' @param shaped_data Output from `shape`.
#' @param ... Further arguments to `\link[rstan]{stan}`.
#' @return A `dgirtfit` object that extends `\link[rstan]{stanfit-class}`. For
#' details see `\link{dgirtfit-class}`.
#' @import rstan
#' @export
#' @include constants.r
dgirt <- function(shaped_data, ...) {

  dots <- list(...,
               file = system.file("inst/dgirt.stan", package = "dgirt", mustWork = TRUE),
               data = shaped_data$as_list(...))
  if (!length(dots$pars)) {
    dots$pars <- c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
              "nu_geo", "nu_geo_prior", "kappa", "sd_item", "sd_theta",
              "sd_theta_bar", "sd_gamma", "sd_innov_gamma", "sd_innov_delta",
              "sd_innov_logsd", "sd_total", "theta_l2", "var_theta_bar_l2")
  }
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }
  dots <- dots[!names(dots) %in% dgirt_pars]

  stanfit <- do.call(rstan::stan, dots)

  tryCatch(new("dgirtFit", stanfit, dgirt_in = shaped_data, call = match.call()),
           error = function(e) {
             warning("Error constructing dgirtfit; returning stanfit object instead")
             stanfit
           })
}
