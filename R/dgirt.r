#' Estimate a DGIRT model
#'
#' @param shaped_data Output of `shape()`.
#' @return The same `stanfit-class` object returned by `rstan::stan()`.
#' @import rstan
#' @export
dgirt <- function(shaped_data, ...) {

  dots <- list(..., file = "dgirt.stan", data = as.list.dgirtIn(shaped_data))
  if (!length(dots$pars)) {
    dots$pars <- c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
              "nu_geo", "nu_geo_prior", "kappa", "sd_item", "sd_theta",
              "sd_theta_bar", "sd_gamma", "sd_innov_gamma", "sd_innov_delta",
              "sd_innov_logsd", "sd_total", "theta_l2", "var_theta_bar_l2")
  }
  if (!length(dots$init_r)) {
    dots$init_r <- 1L
  }

  message("Started ", date(), ".")
  dgirt_stanfit <- do.call(rstan::stan, dots)
  message("Ended ", date(), ".")

  dgirt_stanfit
}

# use_rstan <- function(dgirt_data,
#                       iter,
#                       chains,
#                       n_warm,
#                       n_thin,
#                       save_pars,
#                       seed,
#                       init_range,
#                       vars,
#                       ...) {
#   message("Running ", n_iter, " iterations in each of ", n_chain, " chains. Thinning at an interval of ",
#           n_thin, " with ", n_warm, " adaptation iterations.")
#   return(stan_out)
# }

# stan_out <- switch(method,
#                    optimize = use_cmdstan(dgirt_data,
#                                           optimize_algorithm,
#                                           iter,
#                                           init_r,
#                                           pars,
#                                           vars))
# use_cmdstan <- function(dgirt_data, optimize_algorithm, n_iter, init_range, save_pars, vars) {
#   dump_dgirt(dgirt_data)
#   stan_args <- paste0("optimize algorithm=", optimize_algorithm, " iter=", n_iter, " init='", init_range,
#     "' data file=", get_dump_path(), " output file=", get_output_path())
#   assertthat::assert_that(assertthat::is.readable(get_dgirt_path()))
#   system2(get_dgirt_path(), stan_args)
#   unlink(get_dump_path())
#   if (file.exists(get_output_path())) {
#     stan_output <- read_cmdstan_output(get_output_path(), save_pars)
#     stan_output <- filter_cmdstan_output(stan_output, save_pars)
#     stan_output <- name_output_dims(stan_output, vars)
#     return(stan_output)
#   } else {
#     warning("cmdstan didn't write an output file; check its output for errors.")
#     return(NULL)
#   }
# }

# read_cmdstan_output <- function(path, save_pars) {
#   message("Reading results from disk.")
#   csv <- scan(path, what = "character", sep = ",", quiet = TRUE, comment.char = "#", multi.line = FALSE)
#   len <- length(csv)
#   param <- csv[1:(len / 2)]
#   value <- csv[(len / 2 + 1):len]
#   assertthat::assert_that(equal_length(param, value))
#   value <- as.numeric(value)
#
#   cmdstan_output <- dplyr::data_frame(param, value)
#   assertthat::assert_that(assertthat::not_empty(cmdstan_output))
#   assertthat::assert_that(assertthat::noNA(cmdstan_output))
#
#   return(cmdstan_output)
# }

# filter_cmdstan_output <- function(stan_output, save_pars) {
#   save_par_regex <- paste0("^(", paste0(save_pars, collapse = "|"), ")(_raw)*[.0-9]*$")
#   stan_output <- stan_output %>% dplyr::filter_(~grepl(save_par_regex, param))
#   if (!assertthat::not_empty(stan_output)) {
#     stop("output does not contain estimates for the parameters in save_pars")
#   }
#
#   output_stubs <- sort(unique(gsub(save_par_regex, "\\1", stan_output$param, perl = TRUE)))
#
#   split_output <- lapply(output_stubs, function(stub) {
#     this_par <- stan_output %>%
#       dplyr::filter_(~grepl(paste0("^", stub, "[0-9.]*$"), param))
#     return(this_par)
#   })
#   names(split_output) <- output_stubs
#   split_output
# }

# dump_dgirt <- function(dgirt_data) {
#   assertthat::assert_that(is.list(dgirt_data))
#   assertthat::not_empty(dgirt_data)
#   dgirt_data$vars <- NULL
#   rstan::stan_rdump(names(dgirt_data), get_dump_path(),
#     envir = list2env(dgirt_data))
# }

# get_dgirt_path <- function() {
#   system.file("dgirt", package = "dgirt", mustWork = TRUE)
# }

# get_output_path <- function() {
#   # Paste separately "/" and "output.csv" to avoid absolute path check
#   paste0(system.file(package = "dgirt"), "/", "output.csv")
# }

# get_dump_path <- function() {
#   # Paste separately "/" and "output.csv" to avoid absolute path check
#   paste0(system.file(package = "dgirt"), "/", "dgirt_data.Rdump")
# }

# get_group_names <- function(dgirt_data) {
#   demo_geo_names <- dimnames(dgirt_data$MMM)[[3]]
#   n_groups <- length(gregexpr("__", demo_geo_names[1])[[1]]) + 1
#   group_names <- tidyr::separate_(data.frame(demo_geo_names),
#     "demo_geo_names", into = paste0("factor", seq.int(1, n_groups)),
#     sep = "__", fill = "right")
#   return(group_names)
# }

# get_t_names <- function(dgirt_data) {
#   dimnames(dgirt_data$MMM)[[1]]
# }
#

# get_q_names <- function(dgirt_data) {
#   gsub("_gt0$", "", dimnames(dgirt_data$MMM)[[2]])
# }
#

# get_p_names <- function(dgirt_data) {
#   gsub("^dgirt_(geo|demo)", "", dimnames(dgirt_data$XX)[[2]])
# }
