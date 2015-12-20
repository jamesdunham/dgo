#' Extract and name parameters
#'
#' `extract_dgirt` is a wrapper for rstan::extract that attaches
#' names to parameters according to the values of the data passed
#' to `run_dgirt`.
#' @param dgirt_out Return value of `run_dgirt`.
#' @return Return value of `rstan::extract` with names attached to its elements.
#' @export
extract_dgirt <- function(dgirt_out) {
  assertthat::assert_that(inherits(dgirt_out, "stanfit"))

  # theta_bar, group means, is T x G
  dgirt_extract <- rstan::extract(dgirt_out)
  dimnames(dgirt_extract$theta_bar)[[2]] <- dgirt_out@.MISC$t_names
  dimnames(dgirt_extract$theta_bar)[[3]] <- dgirt_out@.MISC$group
  # (dgirt_extract$theta_bar)

  # xi, common intercepts, is length-T
  dimnames(dgirt_extract$xi)[[2]] <- dgirt_out@.MISC$t_names
  # (dgirt_extract$xi)

  # gamma, hierarchical params, is T x P
  dimnames(dgirt_extract$gamma)[[2]] <- dgirt_out@.MISC$t_names
  dimnames(dgirt_extract$gamma)[[3]] <- dgirt_out@.MISC$p_names
  # (dgirt_extract$gamma)

  # delta_gamma, weight placed on gamma from prev. period, is length-T
  dimnames(dgirt_extract$delta_gamma)[[2]] <- dgirt_out@.MISC$t_names
  # (dgirt_extract$delta_gamma)

  # delta_tbar is length-T
  dimnames(dgirt_extract$delta_tbar)[[2]] <- dgirt_out@.MISC$t_names
  # (dgirt_extract$delta_tbar)

  # nu_geo, weights on geographic predictors, is T x H
  dimnames(dgirt_extract$nu_geo)[[2]] <- dgirt_out@.MISC$t_names
  # (dgirt_extract$nu_geo)
  # TODO: dimnames(dgirt_extract$nu_geo)[[3]] = > dimnames(ZZ) [[3]] [1] 'Zero'

  # nu_geo_prior is length-Hprior (i.e., length-P)
  # (dgirt_extract$nu_geo_prior)
  # TODO: dimnames(dgirt_extract$nu_geo_prior)[[2]] = > dimnames(ZZ) [[3]] [1] 'Zero'

  # kappa, thresholds, is D x Q
  dimnames(dgirt_extract$kappa)[[2]] <- paste("difficulty",
    1:length(dgirt_extract$kappa[[2]]), sep = "_")
  dimnames(dgirt_extract$kappa)[[3]] <- dgirt_out@.MISC$q_names
  # (dgirt_extract$kappa)

  # sd_item, item SD, is length-Q

  dimnames(dgirt_extract$sd_item)[[2]] <- dgirt_out@.MISC$q_names
  # (dgirt_extract$sd_item)

  # sd_theta, SD by period, is length-T ability
  dimnames(dgirt_extract$sd_theta)[[2]] <- dgirt_out@.MISC$t_names
  # (dgirt_extract$sd_theta)

  # sd_theta_bar, residual sd of group ability means, is length-T
  dimnames(dgirt_extract$sd_theta_bar)[[2]] <- dgirt_out@.MISC$t_names
  # (dgirt_extract$sd_theta_bar)

  # sd_total is length-T
  dimnames(dgirt_extract$sd_total)[[2]] <- dgirt_out@.MISC$t_names
  # (dgirt_extract$sd_total)

  # theta_l2, second-level group abililities, is T x Gl2
  dimnames(dgirt_extract$theta_l2)[[2]] <- dgirt_out@.MISC$t_names
  # TODO: dimnames(dgirt_extract$theta_l2)[[3]] =
  # (dgirt_extract$theta_l2)

  # var_theta_bar_l2 is T x Gl2
  dimnames(dgirt_extract$var_theta_bar_l2)[[2]] <- dgirt_out@.MISC$t_names
  # TODO: dimnames(dgirt_extract$var_theta_bar_l2)[[3]] =
  # (dgirt_extract$var_theta_bar_l2)

  return(dgirt_extract)
}
