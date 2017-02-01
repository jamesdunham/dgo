#' \code{dgirt}: fit a DGIRT model
#'
#' \code{dgirt} makes a call to \code{\link[rstan]{stan}} with the Stan code and
#' data for a DGIRT model.
#'
#' The user will typically pass further arguments to \code{\link[rstan]{stan}}
#' via the \code{...} argument, at a minimum \code{iter} and \code{cores}.
#'
#' By default \code{dgirt} overrides the \code{\link[rstan]{stan}} default for
#' its \code{pars} argument to specify typical DGIRT parameters of interest.
#' \code{dgirt} also sets \code{iter_r} to \code{1L}.
#'
#' @param shaped_data Output from \code{\link{shape}}.
#' @param separate_t Whether smoothing of estimates over time should be
#' disabled. Default \code{FALSE}.
#' @param hierarchical_model Whether a hierarchical model should be used to
#' smooth the group IRT estimates. If set to FALSE, the model will return raw
#' group-IRT model estimates for each group. Default \code{TRUE}.
#' @param delta_tbar_prior_mean Prior mean for \code{delta_tbar}, the normal
#' weight on \code{theta_bar} in the previous period.  Default \code{0.65}.
#' @param delta_tbar_prior_sd Prior standard deviation for \code{delta_bar}.
#' Default \code{0.25}.
#' @param innov_sd_delta_scale Prior scale for \code{sd_innov_delta}, the Cauchy
#' innovation standard deviation of \code{nu_geo} and \code{delta_gamma}.
#' Default \code{2.5}.
#' @param innov_sd_theta_scale Prior scale for \code{sd_innov_theta}, the Cauchy
#' innovation standard deviation of \code{gamma}, \code{xi}, and if
#' \code{constant_item} is \code{FALSE} the item difficulty \code{diff}. Default
#' \code{2.5}.
#' @param version The version of the DGIRT model to use.
#' @param ... Further arguments passed to \code{\link[rstan]{stan}}.
#'
#' @return A \code{\link{dgirtfit-class}} object that extends
#' \code{\link[rstan]{stanfit-class}}.
#' @seealso \code{dgirt} expects \code{shaped_data} created by
#'   \code{\link{shape}} and returns an object of class
#'   \code{\link{dgirtfit-class}}.
#' \code{\link{dgirtfit-class}} \code{\link{shape}}
#'
#' @import rstan
#' @export
#' @include constants.r
dgirt <- function(shaped_data, ..., separate_t = FALSE, delta_tbar_prior_mean =
                  0.65, delta_tbar_prior_sd = 0.25, innov_sd_delta_scale = 2.5,
                  innov_sd_theta_scale = 2.5, version = "2017_01_04",
                  hierarchical_model = TRUE) {

  dots <- list(..., object = stanmodels[[version]], data =
    shaped_data$as_list(separate_t = separate_t, hierarchical_model =
      hierarchical_model, delta_tbar_prior_mean = delta_tbar_prior_mean,
    delta_tbar_prior_sd = delta_tbar_prior_sd, innov_sd_delta_scale =
      innov_sd_delta_scale, innov_sd_theta_scale = innov_sd_theta_scale))

  if (!length(dots$pars)) {
    dots$pars <- default_pars
  }
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }
  dots <- dots[!names(dots) %in% dgirt_pars]

  stanfit <- do.call(rstan::sampling, dots)

  tryCatch(new("dgirt_fit", stanfit, dgirt_in = shaped_data, call =
      match.call()),
    error = function(e) {
      warning("Error constructing dgirt_fit; returning stanfit object instead")
      stanfit
    })
}

models <- function() {
  names(stanmodels)
}
