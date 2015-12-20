#' Estimate dynamic group-level IRT model
#'
#' @param dgirt_data Data prepared for use with \code{run_dgirt} by \code{format_data}.
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
#'        can be used if `CmdStan` is available. Note that these methods
#'        are faster than MCMC sampling but return only point estimates.
#'        See \url{http://mc-stan.org/interfaces/cmdstan.html} for `CmdStan`
#'        installation instructions.
#' @return An object of S4 class `stanfit` as returned by `rstan::stan`.
#' @import rstan
#' @export
run_dgirt <- function(dgirt_data, n_iter = 2000, n_chain = 2, max_save = 2000, n_warm = min(10000,
    floor(n_iter * 3 / 4)), n_thin = ceiling((n_iter - n_warm) / (max_save / n_chain)), init_range = 1,
  seed = 1, save_pars = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar", "nu_geo",
    "nu_geo_prior", "kappa", "sd_item", "sd_theta", "sd_theta_bar", "sd_gamma", "sd_innov_gamma",
    "sd_innov_delta", "sd_innov_logsd", "sd_total", "theta_l2", "var_theta_bar_l2"),
  parallel = TRUE, method = "rstan") {

  requireNamespace("rstan", quietly = TRUE)
  rstan::rstan_options(auto_write = parallel)
  if (parallel) {
      options(mc.cores = parallel::detectCores())
  }

  assertthat::assert_that(is_subset(method, c("rstan", "optimize")))
  message("Started: ", date())
  stan_out <- switch(method,
    "rstan" = use_rstan(dgirt_data, n_iter, n_chain, n_warm, n_thin, save_pars, seed, init_range),
    "optimize" = use_cmdstan(dgirt_data, n_iter, init_range, save_pars))
  message("Ended: ", date())

  return(stan_out)
}

use_rstan <- function(dgirt_data, n_iter, n_chain, n_warm, n_thin, save_pars, seed, init_range) {
  message("Running ", n_iter, " iterations in each of ", n_chain, " chains. Thinning at an interval of ",
    n_thin, " with ", n_warm, " adaptation iterations.")
  stan_out <- rstan::stan(model_code = stan_code, data = dgirt_data, iter = n_iter,
    chains = n_chain, warmup = n_warm, thin = n_thin, verbose = FALSE, pars = save_pars,
    seed = seed, init = "random", init_r = init_range)
  stan_out <- attach_names(stan_out, dgirt_data)
  return(stan_out)
}

use_cmdstan <- function(dgirt_data, method, n_iter, init_range, save_pars) {
  dump_dgirt(dgirt_data)
  stan_args <- paste0(method, " iter=", n_iter, " init='", init_range,
    "' data file=", get_dump_path(), " output file=", get_output_path())
  system2(get_dgirt_path(), stan_args)
  unlink(get_dump_path())
  if (file.exists(get_output_path())) {
    stan_output <- read_cmdstan_output(get_output_path())
    stan_output <- name_cmdstan_output(stan_output, dgirt_data, save_pars)
    return(stan_output)
  } else {
    warning("cmdstan didn't write an output file; check its output for errors.")
    return(NULL)
  }
}

read_cmdstan_output <- function(path) {
  message("Reading results from disk.")
  cmdstan_output <- data.table::fread(path, skip = "lp__", sep = ",",
    header = FALSE)
  assertthat::not_empty(cmdstan_output)
  return(cmdstan_output)
}

name_cmdstan_output <- function(stan_output, dgirt_data, save_pars) {
  output_names <- dimnames(stan_output)[[2]]
  par_regex <- paste0("^(", paste0(save_pars, collapse = "|"), ")(_raw)*[.0-9]*$")
  save_pars_matches <- grep(par_regex, output_names, perl = TRUE, value = TRUE)
  stan_output <- stan_output %>% dplyr::select_(~one_of(save_pars_matches))
  parname_stubs <- sort(unique(gsub(par_regex, "\\1", save_pars_matches, perl = TRUE)))

  stan_output <- lapply(parname_stubs, function(parname) {
    this_par <- stan_output %>%
      dplyr::select_(~matches(paste0("^", parname, "[0-9.]*$"))) %>%
      reshape2::melt(id.vars = NULL, variable.name = "param") %>%
      dplyr::as.tbl()
    if (nrow(this_par) > 1) {
      this_par <- this_par %>%
        dplyr::mutate_(index = ~regmatches(as.character(param),
          regexpr("(?<=\\.)([0-9.]*)$", as.character(param), perl = TRUE)))
    }
    return(this_par)
  })
  names(stan_output) <- parname_stubs

  stan_output$kappa$d <- stan_output$kappa$index_1

  t_names <- get_t_names(dgirt_data)
  stan_output$delta_gamma$t <- t_names
  stan_output$delta_tbar$t <- t_names
  stan_output$sd_total$t <- t_names
  stan_output$nu_geo$t <- t_names
  stan_output$sd_theta_bar$t <- t_names
  stan_output$theta_l2$t <- t_names
  stan_output$var_theta_bar_l2$t <- t_names
  stan_output$xi$t <- t_names

  hier_names <- dimnames(dgirt_data$XX)[[2]]
  stan_output$gamma$t <- rep(t_names, length(hier_names))
  stan_output$gamma$p <- rep(hier_names, each = length(t_names))

  q_names <- get_q_names(dgirt_data)
  stan_output$kappa$q <- q_names
  stan_output$sd_item$q <- q_names
  # stan_output$nu_geo$geo <- # T x H geographic predictor

  group_names <- get_group_names(dgirt_data)
  x_collapsed_group_names <- names(dgirt_data$n_vec)
  stan_output$theta_bar <- name_group_means(stan_output$theta_bar, group_names,
    x_collapsed_group_names, t_names)

  return(stan_output)
}

dump_dgirt <- function(dgirt_data) {
  assertthat::assert_that(is.list(dgirt_data))
  assertthat::not_empty(dgirt_data)
  dgirt_data$items <- NULL
  dgirt_data$groups <- NULL
  dgirt_data$time_id <- NULL
  dgirt_data$geo_id <- NULL
  dgirt_data$survey_id <- NULL
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
  n_groups <- length(gregexpr("_x_", demo_geo_names[1])[[1]]) + 1
  group_names <- tidyr::separate_(data.frame(demo_geo_names),
    "demo_geo_names", into = paste0("factor", seq.int(1, n_groups)),
    sep = "_x_", fill = "right")

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

name_group_means <- function(thetas, group_names, x_collapsed_group_names, t_names) {
  thetas$t <- rep(t_names, nrow(group_names))
  thetas <- dplyr::bind_cols(thetas,
    group_names[rep(seq_len(nrow(group_names)), each = length(t_names)), ])
  group_regex <- gregexpr("(?<=_x_)([A-Za-z0-9_]+)(?=_x_[A-Za-z0-9_]+)",
    x_collapsed_group_names, perl = TRUE)
  group_combos <- unique(unlist(regmatches(x_collapsed_group_names, group_regex)))
  group_crosswalk <- dplyr::data_frame(original = group_combos,
    observed = sub("_x_", "_", group_combos, fixed = TRUE))
  group_count <- length(gregexpr("_x_", group_crosswalk$original[1])) + 1
  thetas <- dplyr::left_join(thetas,
    group_crosswalk, by = c("factor1" = "observed")) %>% dplyr::as.tbl() %>%
      dplyr::select_(~-factor1) %>%
      dplyr::select_(~matches(".*"), "geo" = ~matches("^factor")) %>%
      tidyr::separate_("original", into = paste0("group_", seq(1, group_count)),
    sep = "_x_")
      return(thetas)
}

attach_names <- function(stanfit, dgirt_data) {
  if (is.null(stanfit)) {
    warning("stan returned NULL")
  } else {
    stanfit@.MISC$group <- dimnames(dgirt_data$MMM)[[3]]
    stanfit@.MISC$group_names <- get_group_names(dgirt_data)
    stanfit@.MISC$t_names <- get_t_names(dgirt_data)
    stanfit@.MISC$q_names <- get_q_names(dgirt_data)
    stanfit@.MISC$p_names <- get_p_names(dgirt_data)
  }
  return(stanfit)
}
