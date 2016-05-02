#' \code{dgirt}: fit a DGIRT model
#'
#' \code{dgirt} makes a call to \code{\link[rstan]{stan}} with the Stan code and
#' data for a DGIRT model. By default the call overrides the
#' \code{\link[rstan]{stan}} defaults for \code{pars} to specify typical DGIRT
#' parameters of interest and sets \code{iter_r} to \code{1L}.
#'
#' @param shaped_data Output from \code{\link{shape}}.
#'
#' @param separate_t Whether smoothing of estimates over time should be
#' disabled. Default \code{FALSE}.
#'
#' @param delta_tbar_prior_mean Prior mean for \code{delta_tbar}, the normal
#' weight on \code{theta_bar} in the previous period.  Default \code{0.5}.
#'
#' @param delta_tbar_prior_sd Prior standard deviation for \code{delta_bar}.
#' Default \code{0.5}.
#'
#' @param innov_sd_delta_scale Prior scale for \code{sd_innov_delta}, the Cauchy
#' innovation standard deviation of \code{nu_geo} and \code{delta_gamma}.
#' Default \code{2.5}.
#'
#' @param innov_sd_theta_scale Prior scale for \code{sd_innov_theta}, the Cauchy
#' innovation standard deviation of \code{gamma}, \code{xi}, and if
#' \code{constant_item} is \code{FALSE} the item difficulty \code{diff}. Default
#' \code{2.5}.
#'
#' @param ... Further arguments passed to \code{\link[rstan]{stan}}.
#'
#' @return A \code{\link{dgirtfit-class}} object that extends
#' \code{\link[rstan]{stanfit-class}}.
#'
#' @seealso \code{\link{dgirtfit-class}} \code{\link{shape}}
#'
#' @import rstan
#' @export
#' @include constants.r
dgirt <- function(shaped_data, separate_t = FALSE, delta_tbar_prior_mean = 0.5,
                  delta_tbar_prior_sd = 0.5, innov_sd_delta_scale = 2.5,
                  innov_sd_theta_scale = 2.5, ...) {

  dots <- list(...,
               file = system.file("dgirt.stan", package = "dgirt", mustWork = TRUE),
               data = shaped_data$as_list(separate_t = separate_t,
                                          delta_tbar_prior_mean =
                                            delta_tbar_prior_mean,
                                          delta_tbar_prior_sd =
                                            delta_tbar_prior_sd,
                                          innov_sd_delta_scale =
                                            innov_sd_delta_scale,
                                          innov_sd_theta_scale =
                                            innov_sd_theta_scale))
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

  tryCatch(new("dgirtfit", stanfit, dgirt_in = shaped_data, call = match.call()),
           error = function(e) {
             warning("Error constructing dgirtfit; returning stanfit object instead")
             stanfit
           })
}
