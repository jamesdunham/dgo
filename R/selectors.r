#  
# .data = state_opinion
# ids = ids("state", "year", "source")
# items = c("Q_cces2006_minimumwage", "Q_cces2006_abortion")
# control = controls(groups = 'race')
# x = shape(items(.data = .data, ids = ids, items = items, control = control))
#
# 
#' Define item response data 
#' @export
items <- function(.data, ids, items, control, hierarchical = NULL, targets = NULL, item_filter = NULL) {
  item <- Item$new()
  item$tbl <- data.table::as.data.table(.data)
  vars <- names(.data)

  if (is.character(ids)) ids <- ids(ids)
  item$geo <- ids$geo
  item$time <- ids$time
  item$survey <- ids$survey

  item$items <- new("ItemVar", items)

  if (length(hierarchical) > 0) item$modifier <- hierarchical

  if (length(targets) > 0) {
    item$targets <- targets
  } else {
    item$targets <- Target$new()
  }
  if (!length(targets$weight) > 0) {
    item$tbl$weight_ <- 1L
    item$targets$weight <- new("ItemVar", "weight_")
  }

  if (length(item_filter) > 0) {
    item$filters <- item_filter
  } else {
    item$filters <- Filter$new()
  }
  if (!length(item$filters$time) > 0) {
    item$filters$time <- new("TimeFilter", unique(item$tbl[[item$time]]))
  }
  if (!length(item$filters$geo) > 0) {
    item$filters$geo <- new("GeoFilter", levels(unlist(item$tbl[[item$geo]])))
  }
  if (!length(item$filters$min_t) > 0) item$filters$min_t <- 1L
  if (!length(item$filters$min_survey) > 0) item$filters$min_survey <- 1L

  if (length(control) > 0) {
    item$control <- control
  } else {
    item$control <- controls()
  }
  if (!length(item$control) > 0) stop("argument \"groups\" is missing, with no default")
  # TODO: if variables aren't set, then look for variables with the field names in .data
  # TODO: move variable selection to field setters
  item
}

#' Define identifier variables for item response data 
#' @export
ids <- function(geo, time, survey) {
  geo <- new("ItemVar", geo)
  time <- new("ItemVar", time)
  survey <- new("ItemVar", survey)
  list(geo = geo, time = time, survey = survey)
}

#' Define hierarchical data for item response data 
#' @export
hierarchical <- function(.data, modifiers, t1_modifiers, time = NULL, geo = NULL) {
  modifier <- Modifier$new()
  modifier$tbl <- .data
  vars <- names(.data)
  modifier$modifiers <- new("ItemVar", modifiers)
  if (!missing(t1_modifiers)) {
    modifier$t1_modifiers <- new("ItemVar", t1_modifiers)
  } else {
    modifier$t1_modifiers <- modifier$modifiers
  }
  if (!missing(time)) {
    modifier$time <- new("ItemVar", time)
  }
  if (!missing(geo)) {
    modifier$geo <- new("ItemVar", geo)
  }
  modifier
}

#' Define filters for item response data 
#' @export
item_filter <- function(times = NULL, geo = NULL, min_t = NULL, min_survey = NULL) {
  item_filters <- Filter$new()
  item_filters$time <- new("TimeFilter", times)
  item_filters$geo <- new("GeoFilter", geo)
  item_filters$min_t <- min_t
  item_filters$min_survey <- min_survey
  item_filters
}

#' Define population targets for weighting item response data 
#' @export
targets <- function(.data, strata, weight, prop, geo, time) {
  item_targets <- Target$new()
  item_targets$tbl <- .data
  vars <- names(.data)
  item_targets$strata <- new("ItemVar", strata)
  if (!missing(weight)) item_targets$weight <- new("ItemVar", weight)
  if (!missing(prop)) item_targets$prop <- new("ItemVar", prop)
  if (!missing(time)) modifier$time <- new("ItemVar", time)
  if (!missing(geo)) modifier$geo <- new("ItemVar", geo)
  item_targets
}

#' Define dgirt parameters and modeling choices
#' @export
controls <- function(groups, separate_t = FALSE, constant_item = TRUE, delta_tbar_prior_mean = 0.5, delta_tbar_prior_sd = 0.5,
  innov_sd_delta_scale = 2.5, innov_sd_theta_scale = 2.5) {
  control <- Control$new()
  control$groups <- groups
  control$separate_t <- separate_t
  control$constant_item <- constant_item
  control$delta_tbar_prior_mean <- delta_tbar_prior_mean
  control$delta_tbar_prior_sd <- delta_tbar_prior_sd
  control$innov_sd_delta_scale <- innov_sd_delta_scale
  control$innov_sd_theta_scale <- innov_sd_theta_scale
  control
}

handle <- function(ex) {
  res = ifelse(is.symbol(lazyeval::lazy(ex)$expr), lazyeval::lazy(ex), ex)
  as.character(unlist(res))
}
