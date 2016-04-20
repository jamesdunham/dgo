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
#
# read_cmdstan_output <- function(path, save_pars) {
#   message("Reading results from disk.")
#   csv <- scan(path, what = "character", sep = ",", quiet = TRUE, comment.char = "#", multi.line = FALSE)
#   len <- length(csv)
#   param <- csv[1:(len / 2)]
#   value <- csv[(len / 2 + 1):len]
#   assertthat::assert_that(equal_length(param, value))
#   value <- as.numeric(value)
#
#   cmdstan_output <- setDT(data.frame(param, value, stringsAsFactors = FALSE))
#   assertthat::assert_that(assertthat::not_empty(cmdstan_output))
#   assertthat::assert_that(assertthat::noNA(cmdstan_output))
#
#   return(cmdstan_output)
# }
#
# filter_cmdstan_output <- function(stan_output, save_pars) {
#   save_par_regex <- paste0("^(", paste0(save_pars, collapse = "|"), ")(_raw)*[.0-9]*$")
#   stan_output[grepl(save_par_regex, param)]
#   # stan_output <- stan_output %>% dplyr::filter_(~grepl(save_par_regex, param))
#   if (!assertthat::not_empty(stan_output)) {
#     stop("output does not contain estimates for the parameters in save_pars")
#   }
#
#   output_stubs <- sort(unique(gsub(save_par_regex, "\\1", stan_output$param, perl = TRUE)))
#
#   split_output <- lapply(output_stubs, function(stub) {
#     this_par <- copy(stan_output)[grepl(paste0("^", stub, "[0-9.]*$"), param)]
#     # this_par <- stan_output %>%
#     #   dplyr::filter_(~grepl(paste0("^", stub, "[0-9.]*$"), param))
#     return(this_par)
#   })
#   names(split_output) <- output_stubs
#   split_output
# }
#
# dump_dgirt <- function(dgirt_data) {
#   assertthat::assert_that(is.list(dgirt_data))
#   assertthat::not_empty(dgirt_data)
#   dgirt_data$vars <- NULL
#   rstan::stan_rdump(names(dgirt_data), get_dump_path(),
#     envir = list2env(dgirt_data))
# }
#
# get_dgirt_path <- function() {
#   system.file("dgirt", package = "dgirt", mustWork = TRUE)
# }
#
# get_output_path <- function() {
#   # Paste separately "/" and "output.csv" to avoid absolute path check
#   paste0(system.file(package = "dgirt"), "/", "output.csv")
# }
#
# get_dump_path <- function() {
#   # Paste separately "/" and "output.csv" to avoid absolute path check
#   paste0(system.file(package = "dgirt"), "/", "dgirt_data.Rdump")
# }
