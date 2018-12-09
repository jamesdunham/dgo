#' Fit a dynamic group IRT or single-issue MRP model
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
#' Important: the \code{dgirt} model assumes consistent coding of the polarity
#' of item responses for identification.
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
#' @param version The name of the dgo model to estimate, or the path to a
#' \code{.stan} file.  Valid names for dgo models are "2017_01_04",
#' "2017_01_04_singleissue". Ignored if argument \code{model} is used.
#' @param hierarchical_model Whether a hierarchical model should be used to
#' smooth the group IRT estimates. If set to FALSE, the model will return raw
#' group-IRT model estimates for each group. Default \code{TRUE}.
#' @param model A Stan model object of class \code{stanmodel} to be used in
#' estimation. Specifying this argument avoids repeated model compilation. Note
#' that the Stan model object for a model fitted with \code{dgirt()} or
#' \code{dgmrp()} can be found in the the \code{stanmodel} slot of the resulting
#' \code{dgirt_fit} or \code{dgmrp_fit} object.
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
  hierarchical_model = TRUE,
  model = NULL) {

  fit_dgo_model(model_output = "dgirt_fit",
    shaped_data = shaped_data,
    ...,
    separate_t = separate_t,
    delta_tbar_prior_mean = delta_tbar_prior_mean,
    delta_tbar_prior_sd = delta_tbar_prior_sd,
    innov_sd_delta_scale = delta_tbar_prior_sd,
    innov_sd_theta_scale = innov_sd_theta_scale,
    version = version,
    hierarchical_model = hierarchical_model,
    model = model)
}

#'
#' @export
dgmrp <- function(shaped_data,
  ...,
  separate_t = FALSE,
  delta_tbar_prior_mean = 0.65,
  delta_tbar_prior_sd = 0.25,
  innov_sd_delta_scale = 2.5,
  innov_sd_theta_scale = 2.5,
  version = "2017_01_04_singleissue",
  model = NULL) {

  fit_dgo_model(model_output = "dgmrp_fit",
    shaped_data = shaped_data,
    ...,
    separate_t = separate_t,
    delta_tbar_prior_mean = delta_tbar_prior_mean,
    delta_tbar_prior_sd = delta_tbar_prior_sd,
    innov_sd_delta_scale = delta_tbar_prior_sd,
    innov_sd_theta_scale = innov_sd_theta_scale,
    version = version,
    hierarchical_model = TRUE,
    model = model)
}


fit_dgo_model <- function(model_output, shaped_data, ..., separate_t,
  delta_tbar_prior_mean, delta_tbar_prior_sd, innov_sd_delta_scale,
  innov_sd_theta_scale, version, hierarchical_model, model) {
  stopifnot(model_output %in% c("dgirt_fit", "dgmrp_fit"))
  stopifnot(inherits(shaped_data, "dgirtIn"))

  dots <- list(..., data =
    shaped_data$as_list(separate_t = separate_t,
      hierarchical_model = hierarchical_model,
      delta_tbar_prior_mean = delta_tbar_prior_mean,
      delta_tbar_prior_sd = delta_tbar_prior_sd,
      innov_sd_delta_scale = innov_sd_delta_scale,
      innov_sd_theta_scale = innov_sd_theta_scale))

  if (!length(model)) {
    # No precompiled stanmodel object was given
    model_file <- system.file(paste0("models/", version, ".stan"), package = "dgo")
    if (model_file == "") {
      # dgo model not found
      if (file.exists(version)) {
        model_file <- version
      } else {
        stop("'version' should give the name of a model included with dgo ",
          "(see `?dgirt`) or the path to a .stan file")
      }
    }
    stanc_ret = rstan::stanc(file = model_file)
    message("Compiling model...")
    model = rstan::stan_model(stanc_ret = stanc_ret)
    message("Done.")
  } else {
    stopifnot(inherits(model, "stanmodel"))
  }
  dots$object = model

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
      warning("Error constructing dgo_fit; returning stanfit object instead: ",
        e)
      stanfit
  })
}

