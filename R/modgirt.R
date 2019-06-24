#' Fit a multidimensional, ordinal, dynamic group IRT model
#'
#' @param shaped_data Data shaped for the model using
#' \code{\link{shape_modgirt}}.
#' @param version The name of the modgirt model to estimate, or the path to a
#' \code{.stan} file. Ignored if argument \code{model} is used.
#' @param model A Stan model object of class \code{stanmodel} to be used in
#' estimation. Specifying this argument avoids repeated model compilation. Note
#' that the Stan model object for a model fitted with \code{modgirt()} 
#' can be found in the the \code{stanfit} slot of the resulting
#' \code{modgirt_fit} object.
#' @param ... Further arguments, passed to \code{\link[rstan]{stan}}.
#' @return A \code{\link{modgirt_fit-class}} object.
#' @import rstan
#' @include constants.r
#' @export
modgirt <- function(shaped_data,
  version = "2019_01_10_modgirt",
  model = NULL,
  evolving_alpha = FALSE,
  ...) {

  shaped_data@stan_data$evolving_alpha = as.integer(evolving_alpha)

  # Dots will be passed to RStan
  dots <- list(..., data = shaped_data@stan_data)
  # Add a model object
  dots$object = resolve_model(model, version)

  # Unless otherwise specified, use init_r=1
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }

  # Unless otherwise specified, monitor the default parameters
  if (!length(dots$pars)) {
    dots$pars <- modgirt_pars
  }

  # Fit the model
  stanfit <- do.call(rstan::sampling, dots)

  tryCatch(new('modgirt_fit',
      items = shaped_data@items,
      time = shaped_data@time,
      geo = shaped_data@geo,
      demo = shaped_data@demo,
      stan_data_dimnames = dimnames(shaped_data@stan_data$SSSS),
      call = match.call(),
      stanfit = stanfit),
    error = function(e) {
      warning("Error constructing modgirt_fit; returning stanfit object instead: ", e)
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
modgirt_pars <- c(
  "bar_theta",                          # group means
  "alpha",                              # thresholds (difficulty)
  "beta",                               # discrimination
  "sd_theta",                           # within-group SD of theta
  "sd_bar_theta_evolve",                # transition SD of bar_theta
  "sd_alpha_evolve",                    # transition SD of alpha
  "sd_xi_evolve",                       # transition SD of xi
  "sd_gamma_evolve"                     # transition SD of gamma/delta_tbar
)
if (length(shaped_data@items) == 1) modgirt_pars <- c(modgirt_pars, "PI")

