#' Summarize dgirt samples
#'
#' `apply_dgirt` applies a scalar function over dgirt sampler iterations for each parameter that appears in the `stanfit`
#' object returned by `dgirt`.
#' 
#' @param dgirt_output Return value of `dgirt`, a `stanfit` object.
#' @param dgirt_input Return value of `wrangle`.
#' @param fun A single scalar function like `mean`.
#' @return A list of tables summarizing the posterior distribution of each model parameter.
#' @export
apply_dgirt <- function(dgirt_output, dgirt_input, fun = mean.default) {
  assertthat::assert_that(inherits(dgirt_output, "stanfit"))
  assertthat::assert_that(assertthat::not_empty(dgirt_input$vars))

  dgirt_extract <- rstan::extract(dgirt_output)
  vars <- dgirt_input$vars

  dgirt_summary = lapply(dgirt_extract, function(element) {
    assertthat::assert_that(assertthat::not_empty(element))
    if (!is.null(dim(element)) && length(dim(element)) > 1) {
      over_dims = seq.int(2, length(dim(element)))
      out = apply(element, over_dims, fun)
    } else {
      out = fun(element)
    }
    if (length(out) > 1) {
      out = reshape2::melt(out)
    }
    return(out)
  })

  dgirt_summary = name_output_dims(dgirt_summary, vars)

  return(dgirt_summary)
}

#' Extract and name parameters
#'
#' `name ` is a wrapper for rstan::extract that attaches names to parameters
#' using the data originally passed to `dgirt`.
#' @param dgirt_output Return value of `dgirt`, a `stanfit` object.
#' @param dgirt_input Return value of `wrangle`.
#' @return What `rstan::extract` returns, but with dimension names.
#' @export
name_pars <- function(dgirt_output, dgirt_input) {
  assertthat::assert_that(inherits(dgirt_output, "stanfit"))
  assertthat::assert_that(assertthat::not_empty(dgirt_input$vars))

  dgirt_extract <- rstan::extract(dgirt_output)

  vars <- dgirt_input$vars
  dim2_indexed_t <- c('theta_bar', 'xi', 'gamma', 'delta_gamma', 'delta_tbar', 'nu_geo', 'sd_theta', 'sd_theta_bar',
    'sd_total', 'theta_l2', 'var_theta_bar_l2')
  if (!as.logical(dgirt_input$constant_item)) dim2_indexed_t <- c(dim2_indexed_t, "kappa")

  for (i in dim2_indexed_t) {
    names(attributes(dgirt_extract[[i]])$dimnames)[2] <- 'time'
    assertthat::assert_that(identical(dim(dgirt_extract[[i]])[2], length(vars$use_t)))
    dimnames(dgirt_extract[[i]])[[2]] <- vars$use_t
  }

  names(attributes(dgirt_extract[['theta_bar']])$dimnames)[3] <- 'group'
  groups_concat = concat_groups(vars$covariate_groups, vars$groups, vars$geo_id, "groups")$groups
  assertthat::assert_that(identical(dim(dgirt_extract[['theta_bar']])[3], length(groups_concat)))
  dimnames(dgirt_extract[['theta_bar']])[[3]] <- groups_concat

  names(attributes(dgirt_extract[['gamma']])$dimnames)[3] <- 'param'
  assertthat::assert_that(identical(dim(dgirt_extract[['gamma']])[3], length(vars$hier_names)))
  dimnames(dgirt_extract[['gamma']])[[3]] <- vars$hier_names

  names(attributes(dgirt_extract[['kappa']])$dimnames)[3] <- 'item'
  assertthat::assert_that(identical(dim(dgirt_extract[['kappa']])[3], length(vars$gt_items)))
  dimnames(dgirt_extract[['kappa']])[[3]] <- vars$gt_items

  names(attributes(dgirt_extract[['sd_item']])$dimnames)[2] <- 'item'
  assertthat::assert_that(identical(dim(dgirt_extract[['sd_item']])[2], length(vars$gt_items)))
  dimnames(dgirt_extract[['sd_item']])[[2]] <- vars$gt_items

  dgirt_extract
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


