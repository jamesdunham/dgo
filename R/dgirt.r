#' \code{dgirt} and \code{dgmrp}: fit a DGIRT or single-issue MRP model
#'
#' \code{dgirt} and \code{dgmrp} make calls to \code{\link[rstan]{stan}} with
#' the Stan code and data for their respective models.
#'
#' The user will typically pass further arguments to \code{\link[rstan]{stan}}
#' via the \code{...} argument, at a minimum \code{iter} and \code{cores}.
#'
#' By default \code{dgirt} and \code{dgmrp} override the
#' \code{\link[rstan]{stan}} default for its \code{pars} argument to specify
#' typical parameters of interest. They also set \code{iter_r} to \code{1L}.
#'
#' @param shaped_data Output from \code{\link{shape}}.
#' @param ... Further arguments, passed to \code{\link[rstan]{stan}}.
#' @param separate_t Whether smoothing of estimates over time should be
#' disabled. Default \code{FALSE}.
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
#' @param version The model version to use, as the name of a \code{.stan} file
#' installed with the package from the \code{exec} directory of the source.
#' @param hierarchical_model Whether a hierarchical model should be used to
#' smooth the group IRT estimates. If set to FALSE, the model will return raw
#' group-IRT model estimates for each group. Default \code{TRUE}.
#'
#' @return A \code{\link{dgo_fit-class}} object that extends
#' \code{\link[rstan]{stanfit-class}}.
#'
#' @aliases dgmrp
#' @import rstan
#' @include constants.r
#' @export
dgirt <- function(shaped_data,
  ...,
  separate_t = FALSE,
  delta_tbar_prior_mean = 0.65,
  delta_tbar_prior_sd = 0.25,
  innov_sd_delta_scale = 2.5,
  innov_sd_theta_scale = 2.5,
  version = "2017_01_04",
  hierarchical_model = TRUE) {
  fit_dgo_model("dgirt_fit",
    shaped_data = shaped_data,
    ...,
    separate_t = separate_t,
    delta_tbar_prior_mean = delta_tbar_prior_mean,
    delta_tbar_prior_sd = delta_tbar_prior_sd,
    innov_sd_delta_scale = delta_tbar_prior_sd,
    innov_sd_theta_scale = innov_sd_theta_scale,
    version = version,
    hierarchical_model = hierarchical_model)
}

#' @rdname dgirt
#' @export
dgmrp <- function(shaped_data,
  ...,
  separate_t = FALSE,
  delta_tbar_prior_mean = 0.65,
  delta_tbar_prior_sd = 0.25,
  innov_sd_delta_scale = 2.5,
  innov_sd_theta_scale = 2.5,
  version = "2017_01_04_singleissue") {
  fit_dgo_model("dgmrp_fit",
    shaped_data = shaped_data,
    ...,
    separate_t = separate_t,
    delta_tbar_prior_mean = delta_tbar_prior_mean,
    delta_tbar_prior_sd = delta_tbar_prior_sd,
    innov_sd_delta_scale = delta_tbar_prior_sd,
    innov_sd_theta_scale = innov_sd_theta_scale,
    version = version,
    hierarchical_model = TRUE)
}

fit_dgo_model <- function(model_output, shaped_data, ..., separate_t,
  delta_tbar_prior_mean, delta_tbar_prior_sd, innov_sd_delta_scale,
  innov_sd_theta_scale, version, hierarchical_model) {
  stopifnot(model_output %in% c("dgirt_fit", "dgmrp_fit"))
  stopifnot(inherits(shaped_data, "dgirtIn"))
  stopifnot(version %in% names(stanmodels))
  dots <- list(..., object = stanmodels[[version]], data =
    shaped_data$as_list(separate_t = separate_t,
      hierarchical_model = hierarchical_model,
      delta_tbar_prior_mean = delta_tbar_prior_mean,
      delta_tbar_prior_sd = delta_tbar_prior_sd,
      innov_sd_delta_scale = innov_sd_delta_scale,
      innov_sd_theta_scale = innov_sd_theta_scale))

  if (model_output == "dgmrp_fit" && length(shaped_data$gt_items) > 1) {
    stop("Multiple items in item data. Single-issue MRP models can only ",
      "include one item.")
  }
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }
  if (!length(dots$pars)) {
    if (model_output == "dgmrp_fit") {
      dots$pars <- default_pars_mrp
      dots <- dots[!names(dots) %in% dgmrp_pars]
    } else {
      dots$pars <- default_pars
      dots <- dots[!names(dots) %in% dgirt_pars]
    }
  }

  stanfit <- do.call(rstan::sampling, dots)
  tryCatch(new(model_output, stanfit, dgirt_in = shaped_data, call =
      match.call()),
    error = function(e) {
      warning("Error constructing dgo_fit; returning stanfit object instead")
      stanfit
  })
}

