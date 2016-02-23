# Transitional functions
wrangle_to_shape <- function() {
  arg <- handle_arguments()

  item <- Item$new()
  item$modifier <- Modifier$new()
  item$filters <- Filters$new()
  item$targets <- Targets$new()
  item$control <- Control$new()

  item$tbl <- arg$level1
  item$items <- new("ItemVar", arg$items)
  item$groups <- new("ItemVar", arg$groups)
  item$geo <- new("ItemVar", arg$geo_id)
  item$time <- new("ItemVar", arg$time_id)
  item$survey <- new("ItemVar", arg$survey_id)
  item$weight <- new("ItemVar", arg$survey_weight) 

  if (!length(arg$level2) < 1) {
    item$modifier$tbl <- arg$level2
    item$modifier$modifiers <- new("ItemVar", arg$level2_modifiers)
    item$modifier$t1_modifiers <- new("ItemVar", arg$level2_period1_modifiers)
    item$modifier$time <- new("ItemVar", item$time)
    item$modifier$geo <- new("ItemVar", item$geo)
  }

  item$filters$t <- set_use_t(item, arg)
  item$filters$geo <- update_use_geo(item)
  item$filters$min_t <- arg$min_periods
  item$filters$min_survey <- arg$min_surveys

  if (!length(arg$targets) < 1) {
    item$targets$tbl <- arg$targets
    item$targets$strata <- new("ItemVar", arg$target_groups)
    item$targets$prop <- new("ItemVar", arg$target_proportion)
    item$targets$geo <- new("ItemVar", item$geo)
    item$targets$time <- new("ItemVar", item$time)
  }

  item$control$separate_t <- as.integer(arg$separate_periods)
  item$control$constant_item <- as.integer(arg$constant_item)
  item$control$delta_tbar_prior_mean <- arg$delta_tbar_prior_mean
  item$control$delta_tbar_prior_sd <- arg$delta_tbar_prior_sd
  item$control$innov_sd_delta_scale <- arg$innov_sd_delta_scale
  item$control$innov_sd_theta_scale <- arg$innov_sd_theta_scale

  item
} 

set_use_t <- function(item, arg) {
  if (!length(arg$periods) > 0) {
    return(unique(item$tbl[[item$time]]))
  } else {
    assertthat::assert_that(is.numeric(arg$periods))
    return(arg$periods)
  }
}

update_use_geo <- function(item, arg) {
  levels(unlist(item$tbl[[item$geo]]))
}
