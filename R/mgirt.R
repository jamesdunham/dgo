#' Fit a multinomial group IRT model
#'
#' @param shaped_data Data shaped for the model. A function for accomplishing
#' this is not yet implemented.
#' @param ... Further arguments, passed to \code{\link[rstan]{stan}}.
#' @return A \code{\link{mgirt_fit-class}} object that extends
#' \code{\link[rstan]{stanfit-class}}. (Not yet implemented.)
#'
#' @import rstan
#' @include constants.r
#' @export
mgirt <- function(shaped_data, version = "2018_08_18_mdgirt_ord", model = NULL,
  evolving_alpha = FALSE, ...)  {

 shaped_data$evolving_alpha = as.integer(evolving_alpha)

  # Dots will be passed to RStan
  dots <- list(..., data = shaped_data)
  # Add a model object
  dots$object = resolve_model(model, version)

  # Unless otherwise specified, use init_r=1
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }

  # Unless otherwise specified, monitor the default parameters
  if (!length(dots$pars)) {
    dots$pars <- mgirt_pars
  }

  # Fit the model
  stanfit <- do.call(rstan::sampling, dots)

  # TODO: create new model class for mgirt models
  tryCatch(new('mgirt_fit', stanfit, call = match.call()),
    error = function(e) {
      warning("Error constructing dgo_fit; returning stanfit object instead: ",
        e)
    stanfit
  })
}

resolve_model <- function(model, version) {
  # If the user passes a stanmodel to 'model', ignore the 'version' argument and
  # use the stanmodel. Otherwise, look for a model file on the disk: (1) by
  # suffixing the value of 'version' with '.stan', and looking for a file of
  # that name in the dgo directory, then (2) checking whether 'version' gives a
  # valid path, and if so compiling that file.
  if (!length(model)) {
    # No precompiled stanmodel object was given
    model_file <- system.file(paste0("models/", version, ".stan"), package = "dgo")
    if (model_file == "") {
      # 'model' doesn't give the name of a packaged model
      if (file.exists(version)) {
        # 'model' seems instead to give the name of a user model, so we'll
        # compile that
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
    # The object passed to model must be a precompiled stanmodel
    stopifnot(inherits(model, "stanmodel"))
  }
  return(model)
}

# Default model parameters to monitor
mgirt_pars <- c( "raw_bar_theta_N01", "raw_alpha", "beta_free",
  "beta_neg", "beta_pos", "sd_theta_N01", "sd_theta_IG", "sd_theta_evolve_N01",
  "sd_theta_evolve_IG", "sd_alpha_evolve_N01", "sd_alpha_evolve_IG",
  "raw_bar_theta", "bar_theta",
  "beta",	            # discrimination
  "alpha",            # thresholds (difficulty)
  "sd_theta",	        # within-group SD of theta
  "sd_theta_evolve",  # transition SD of theta
  "sd_alpha_evolve",	# transition SD of alpha
  "sigma_theta")

