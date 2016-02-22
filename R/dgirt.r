#' Estimate dynamic group-level IRT model
#'
#' @param dgirt_data Data prepared for use with `dgirt` by `wrangle`.
#' @param n_iter See \code{iter} in \code{rstan::stan}.
#' @param n_chain See \code{chains} in \code{rstan::stan}.
#' @param n_warm See \code{warmup} in \code{rstan::stan}.
#' @param max_save Maximum iterations to save; only used in the default value of n_thin.
#' @param n_thin See \code{thin} in \code{rstan::stan}.
#' @param init_range See \code{init} in \code{rstan::stan}.
#' @param seed See \code{seed} in \code{rstan::stan}.
#' @param save_pars See \code{pars} in \code{rstan::rstan_options}
#' @param parallel See \code{rstan::rstan_options(auto_write = parallel)}.
#' @param method By default, `rstan::stan` estimates the model using MCMC
#'        sampling. Alternatively, `cmdstan optimize` or `cmdstan variational`
#'        can be used if CmdStan is available. Note that these methods
#'        are faster than MCMC sampling but return only point estimates.
#'        See \url{http://mc-stan.org/interfaces/cmdstan.html} for CmdStan
#'        installation instructions.
#' @param optimize_algorithm The optimization algorithm for CmdStan to use if
#'        `method` is `"optimize"`, one of `"bfgs"`, `"lbfgs"` (the default),
#'        and `"newton"`. See CmdStan documentation for details.
#' @return An object of S4 class `stanfit` as returned by `rstan::stan`.
#' @import rstan
#' @export
dgirt <- function(dgirt_data, n_iter = 2000, n_chain = 2, max_save = 2000, n_warm = min(10000,
    floor(n_iter * 3 / 4)), n_thin = ceiling((n_iter - n_warm) / (max_save / n_chain)), init_range = 1,
  seed = 1, save_pars = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar", "nu_geo",
    "nu_geo_prior", "kappa", "sd_item", "sd_theta", "sd_theta_bar", "sd_gamma", "sd_innov_gamma",
    "sd_innov_delta", "sd_innov_logsd", "sd_total", "theta_l2", "var_theta_bar_l2"),
  parallel = TRUE, method = "rstan", algorithm = NULL) {

  requireNamespace("rstan", quietly = TRUE)
  rstan::rstan_options(auto_write = parallel)
  if (parallel) {
      options(mc.cores = parallel::detectCores())
  }

  if (length(save_pars) < 1) stop("save_pars gives no parameters to save")

  vars = dgirt_data$vars
  dgirt_data$vars = NULL
  group_counts = dgirt_data$group_counts
  dgirt_data$group_counts = NULL

  assertthat::assert_that(is_subset(method, c("rstan", "optimize", "variational")))
  message("Started: ", date())
  stan_out <- switch(method,
    rstan = use_rstan(dgirt_data, n_iter, n_chain, n_warm, n_thin, save_pars, seed, init_range, vars),
    optimize = use_cmdstan(dgirt_data, method, algorithm = 'lbfgs', n_iter, init_range, save_pars, vars),
    variational = use_cmdstan(dgirt_data, method, algorithm = 'meanfield', n_iter, init_range, save_pars, vars))
  message("Ended: ", date())

  return(stan_out)
}

use_rstan <- function(dgirt_data, n_iter, n_chain, n_warm, n_thin, save_pars, seed, init_range, vars) {
  message("Running ", n_iter, " iterations in each of ", n_chain, " chains. Thinning at an interval of ",
    n_thin, " with ", n_warm, " adaptation iterations.")
  stan_out <- rstan::stan(model_code = stan_code, data = dgirt_data, iter = n_iter,
    chains = n_chain, warmup = n_warm, thin = n_thin, verbose = FALSE, pars = save_pars,
    seed = seed, init = "random", init_r = init_range)
  return(stan_out)
}

use_cmdstan <- function(dgirt_data, method, algorithm, n_iter, init_range, save_pars, vars) {
  dump_dgirt(dgirt_data)
  stan_args <- paste0(method, " algorithm=", algorithm, " iter=", n_iter, " init='", init_range,
    "' data file=", get_dump_path(), " output file=", get_output_path())
  assertthat::assert_that(assertthat::is.readable(get_dgirt_path()))
  system2(get_dgirt_path(), stan_args)
  unlink(get_dump_path())
  if (file.exists(get_output_path())) {
    stan_output <- read_cmdstan_output(get_output_path(), save_pars)
    stan_output <- filter_cmdstan_output(stan_output, save_pars)
    stan_output <- name_output_dims(stan_output, vars)
    return(stan_output)
  } else {
    warning("cmdstan didn't write an output file; check its output for errors.")
    return(NULL)
  }
}

read_cmdstan_output <- function(path, save_pars) {
  message("Reading results from disk.")
  csv <- scan(path, what = "character", sep = ",", quiet = TRUE, comment.char = "#", multi.line = FALSE)
  len <- length(csv)
  param <- csv[1:(len / 2)]
  value <- csv[(len / 2 + 1):len]
  assertthat::assert_that(equal_length(param, value))
  value <- as.numeric(value)

  cmdstan_output <- dplyr::data_frame(param, value)
  assertthat::assert_that(assertthat::not_empty(cmdstan_output))
  assertthat::assert_that(assertthat::noNA(cmdstan_output))

  return(cmdstan_output)
}

filter_cmdstan_output <- function(stan_output, save_pars) {
  save_par_regex <- paste0("^(", paste0(save_pars, collapse = "|"), ")(_raw)*[.0-9]*$")
  stan_output <- stan_output %>% dplyr::filter_(~grepl(save_par_regex, param))
  if (!assertthat::not_empty(stan_output)) {
    stop("output does not contain estimates for the parameters in save_pars")
  }

  output_stubs <- sort(unique(gsub(save_par_regex, "\\1", stan_output$param, perl = TRUE)))

  split_output <- lapply(output_stubs, function(stub) {
    this_par <- stan_output %>%
      dplyr::filter_(~grepl(paste0("^", stub, "[0-9.]*$"), param))
    return(this_par)
  })
  names(split_output) <- output_stubs
  split_output
}

name_output_dims <- function(stan_output, vars) {
  indexed_t = intersect(c("delta_gamma", "delta_tbar", "sd_total", "nu_geo", "sd_theta_bar",
    "theta_l2", "var_theta_bar_l2", "xi"), names(stan_output))
  stan_output[indexed_t] = lapply(stan_output[indexed_t], attach_t,
    vars$use_t, vars$time_id)

  if (length(stan_output$gamma) > 0) {
    stan_output$gamma$t <- rep(vars$use_t, length(vars$hier_names))
    stan_output$gamma$p <- rep(vars$hier_names, each = length(vars$use_t))
  }

  if (length(stan_output$kappa) > 0) {
    stan_output$kappa$q <- vars$gt_items
  }

  if (length(stan_output$sd_item) > 0) {
    stan_output$sd_item$q <- vars$gt_items
  }

  if (length(stan_output$theta_bar) > 0) {
  stan_output$theta_bar <- stan_output$theta_bar %>%
    name_group_means(vars)
  }
  return(stan_output)
}

dump_dgirt <- function(dgirt_data) {
  assertthat::assert_that(is.list(dgirt_data))
  assertthat::not_empty(dgirt_data)
  dgirt_data$vars <- NULL
  rstan::stan_rdump(names(dgirt_data), get_dump_path(),
    envir = list2env(dgirt_data))
}

get_dgirt_path <- function() {
  system.file("dgirt", package = "dgirt", mustWork = TRUE)
}

get_output_path <- function() {
  # Paste separately "/" and "output.csv" to avoid absolute path check
  paste0(system.file(package = "dgirt"), "/", "output.csv")
}

get_dump_path <- function() {
  # Paste separately "/" and "output.csv" to avoid absolute path check
  paste0(system.file(package = "dgirt"), "/", "dgirt_data.Rdump")
}

get_group_names <- function(dgirt_data) {
  demo_geo_names <- dimnames(dgirt_data$MMM)[[3]]
  n_groups <- length(gregexpr("__", demo_geo_names[1])[[1]]) + 1
  group_names <- tidyr::separate_(data.frame(demo_geo_names),
    "demo_geo_names", into = paste0("factor", seq.int(1, n_groups)),
    sep = "__", fill = "right")
  return(group_names)
}

get_t_names <- function(dgirt_data) {
  dimnames(dgirt_data$MMM)[[1]]
}

get_q_names <- function(dgirt_data) {
  gsub("_gt0$", "", dimnames(dgirt_data$MMM)[[2]])
}

get_p_names <- function(dgirt_data) {
  gsub("^dgirt_(geo|demo)", "", dimnames(dgirt_data$XX)[[2]])
}

name_group_means <- function(thetas, vars) {
  assertthat::assert_that(assertthat::not_empty(thetas))
  assertthat::assert_that(assertthat::not_empty(vars))
  assertthat::assert_that(assertthat::not_empty(vars$covariate_groups))
  assertthat::assert_that(assertthat::not_empty(vars$use_t))
  thetas[[vars$time_id]] <- rep(as.numeric(vars$use_t), nrow(vars$covariate_groups))
  thetas <- thetas %>%
    dplyr::bind_cols(vars$covariate_groups[rep(seq_len(nrow(vars$covariate_groups)),
      each = length(vars$use_t)), ])
  thetas <- thetas %>% dplyr::mutate_each_("as.factor", vars = c(vars$groups, vars$geo_id))
  return(thetas)
}

attach_t = function(element, use_t, time_id) {
  assertthat::assert_that(identical(nrow(element), length(use_t)))
  dplyr::mutate_(element, .dots = setNames(list(~use_t), time_id))
}
