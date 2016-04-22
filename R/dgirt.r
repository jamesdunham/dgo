#' Estimate a DGIRT model
#'
#' `dgirt` makes a call to `\link[rstan]{stan}` with the Stan code and data for
#' a DGIRT model. By default the call overrides the `stan` defaults for `pars`
#' to specify typical DGIRT parameters of interest and sets `iter_r` to `1L`.
#'
#' @param shaped_data Output from `shape`.
#' @param ... Further arguments to `\link[rstan]{stan}`.
#' @return A `dgirtfit` object that extends `\link[rstan]{stanfit-class}`. For
#' details see `\link{dgirtfit-class}`.
#' @import rstan
#' @export
dgirt <- function(shaped_data, ...) {

  dots <- list(...,
               file = system.file("R/dgirt.stan", package = "dgirt", mustWork = TRUE),
               data = shaped_data$as_list())
  if (!length(dots$pars)) {
    dots$pars <- c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
              "nu_geo", "nu_geo_prior", "kappa", "sd_item", "sd_theta",
              "sd_theta_bar", "sd_gamma", "sd_innov_gamma", "sd_innov_delta",
              "sd_innov_logsd", "sd_total", "theta_l2", "var_theta_bar_l2")
  }
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }

  stanfit <- do.call(rstan::stan, dots)

  tryCatch(new("dgirtFit", stanfit, dgirt_in = shaped_data),
           error = function(e) {
             warning("Error constructing dgirtfit; returning stanfit object instead")
             stanfit
           })
}

