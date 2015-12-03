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
#' @export
run_dgirt <- function(dgirt_data, n_iter = 2000, n_chain = 2, max_save = 2000, n_warm = min(10000, 
  floor(n_iter * 3/4)), n_thin = ceiling((n_iter - n_warm)/(max_save/n_chain)), 
  init_range = 1, seed = 1, save_pars = c("theta_bar", "xi", "gamma", "delta_gamma", 
    "delta_tbar", "nu_geo", "nu_geo_prior", "kappa", "sd_item", "sd_theta", "sd_theta_bar", 
    "sd_gamma", "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd", "sd_total", 
    "theta_l2", "var_theta_bar_l2"), parallel = TRUE) {
  
  cat(stringr::str_wrap(stringr::str_c("Running ", n_iter, " iterations in each of ", 
    n_chain, " chains, thinned at an interval of ", n_thin, ", with ", n_warm, 
    " adaptation iterations over the years ", rownames(dgirt_data$ZZ)[1], "-", 
    rownames(dgirt_data$ZZ)[length(rownames(dgirt_data$ZZ))], ".")))
  
  rstan::rstan_options(auto_write = parallel)
  if (parallel) {
    options(mc.cores = parallel::detectCores())
  }
  
  cat("\nStart: ", date(), "\n")
  stan.out <- rstan::stan(model_code = stan_code, data = dgirt_data, iter = n_iter, 
    chains = n_chain, warmup = n_warm, thin = n_thin, verbose = FALSE, pars = save_pars, 
    seed = seed, init = "random", init_r = init_range)
  cat("\nEnd: ", date(), "\n")
  return(stan.out)
}
