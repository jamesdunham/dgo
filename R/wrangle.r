# data(state_opinion)
# data = list(level1 = state_opinion)
# vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
#             groups = c("race"),
#             time_id = "year",
#             geo_id = "state",
#             survey_id = "source",
#             survey_weight = "weight")
# filters = list(periods = c(2006:2010))
#
# data(state_opinion)
# data = list(level1 = state_opinion,level2 = dplyr::mutate(state_opinion, education = sample(1:2, nrow(state_opinion), replace = TRUE)) %>% dplyr::distinct(state, year))
# vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
#             groups = c("race"),
#             time_id = "year",
#             geo_id = "state",
#             survey_id = "source",
#             survey_weight = "weight",
#             level2_modifiers = "education",
#             level2_period1_modifiers = "education")
# filters = list(periods = c(2006:2010))
wrangle <- function(data = list(level1,
                                level2 = NULL,
                                targets = NULL),
                       vars = list(items,
                                   groups,
                                   time_id,
                                   geo_id,
                                   survey_id,
                                   survey_weight,
                                   target_groups = NULL,
                                   target_proportion = NULL,
                                   level2_modifiers = NULL,
                                   level2_period1_modifiers = NULL),
                       filters = list(periods = NULL,
                                      geo_ids = NULL,
                                      min_surveys = 1L,
                                      min_periods = 1L),
                       params = list(separate_periods = FALSE,
                                     constant_item = TRUE,
                                     delta_tbar_prior_mean = 0.5,
                                     delta_tbar_prior_sd = 0.5,
                                     innov_sd_delta_scale = 2.5,
                                     innov_sd_theta_scale = 2.5)) {
    item <- wrangle_to_shape()
    stan_data <- shape(item, item$control)
    stan_data
}

wrangle_to_shape <- function() {
  arg <- handle_arguments()
  item <- Item$new()

  item$tbl <- arg$level1

  item$items <- new("ItemVar", arg$items)
  item$geo <- new("ItemVar", arg$geo_id)
  item$time <- new("ItemVar", arg$time_id)
  item$survey <- new("ItemVar", arg$survey_id)
  item$targets$weight <- new("ItemVar", arg$survey_weight)

  if (length(arg$level2) > 0) { item$modifier$tbl <- arg$level2
  item$modifier$modifiers <- new("ItemVar", arg$level2_modifiers)
  item$modifier$t1_modifiers <- new("ItemVar", arg$level2_period1_modifiers)
  item$modifier$time <- new("ItemVar",
                                                                                                       item$time)
  item$modifier$geo <- new("ItemVar", item$geo) }

  item$filters$time <- set_use_t(item, arg)
  item$filters$geo <- update_use_geo(item)
  item$filters$min_t <- arg$min_periods
  item$filters$min_survey <- arg$min_surveys

  if (length(arg$targets) > 0) {
    item$targets$tbl <- arg$targets
    item$targets$strata <- new("ItemVar", arg$target_groups)
    item$targets$prop <- new("ItemVar", arg$target_proportion)
    item$targets$geo <- new("ItemVar", item$geo)
    item$targets$time <- new("ItemVar", item$time)
  }

  item$control$groups <- new("ItemVar", arg$groups)
  item$control$separate_t <- as.integer(arg$separate_periods)
  item$control$constant_item <- as.integer(arg$constant_item)
  item$control$delta_tbar_prior_mean <- arg$delta_tbar_prior_mean
  item$control$delta_tbar_prior_sd <- arg$delta_tbar_prior_sd
  item$control$innov_sd_delta_scale <- arg$innov_sd_delta_scale
  item$control$innov_sd_theta_scale <- arg$innov_sd_theta_scale

  item
}

#' @export
concat_groups <- function(tabular, group_names, geo_id, name) {
  has_all_names(tabular, group_names)
  has_all_names(tabular, geo_id)
  tabular %>%
    tidyr::unite_("group_concat", group_names, sep = "_") %>%
    tidyr::unite_(name, c("group_concat", geo_id), sep = "_x_")
}

#' @export
split_groups <- function(tabular, group_names, geo_id, name) {
  assertthat::assert_that(has_name(tabular, "name"))
  tabular %>%
    tidyr::separate_(name, c("group_concat", geo_id), sep = "_x_") %>%
    tidyr::separate_("group_concat", group_names, sep = "_")
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

handle_arguments <- function() {
  arg <- mget(names(formals(wrangle)), parent.frame(2L), ifnotfound = list(rep(NULL, length(formals(wrangle)))))
  arg <- unlist(arg, recursive = FALSE)
  assertthat::not_empty(names(arg))
  names(arg) <- sub("^(data|vars|filters|params)\\.", "", names(arg))
  arg <- set_arg_defaults(arg)
  check_arg_lengths(arg)
  check_arg_names(arg)
  check_arg_types(arg)
  check_arg_ranges(arg)
  arg$min_periods <- as.integer(arg$min_periods)
  arg$min_surveys <- as.integer(arg$min_surveys)
  return(arg)
}

check_arg_lengths <- function(arg) {
  if (!length(arg$items) > 0) stop("at least one item variable required ('items')")
  if (!length(arg$groups) > 0) stop("at least one grouping variable required ('groups'); NB: until future version")
  if (!identical(length(arg$time_id), 1L)) stop("single time identifier required ('time_id')")
  if (!identical(length(arg$geo_id), 1L)) stop("single geographic identifier required ('geo_id')")
  if (!identical(length(arg$survey_id), 1L)) stop("single survey identifier required ('survey_id')")
  if (!identical(length(arg$survey_weight), 1L)) stop("single survey weight variable required ('survey_weight')")
  if (length(arg$level2) > 0) {
    if (!length(arg$level2_modifiers) > 0) stop("at least one modifier variable ('level2_modifier') required with hierarchical data ('level2')")
    if (!length(arg$level2_period1_modifiers) > 0) stop("at least one first-period modifier variable ('level2_period1_modifier') required with hierarchical data ('level2')")
  }
  if (length(arg$targets) > 0) {
    if (!identical(length(arg$target_proportion), 1L)) stop("single proportion variable ('target_proportion') required with target data ('targets')")
    if (!length(arg$target_groups) > 0) stop("at least one stratifying variable ('target_groups') required with target data ('targets')")
  }
  return(TRUE)
}

# Check that names given in arguments appear in data.frame arguments
check_arg_names <- function(arg) {
  assertthat::assert_that(has_all_names(arg$level1, arg$items))
  assertthat::assert_that(has_all_names(arg$level1, arg$groups))
  assertthat::assert_that(assertthat::has_name(arg$level1, arg$time_id))
  assertthat::assert_that(assertthat::has_name(arg$level1, arg$geo_id))
  assertthat::assert_that(assertthat::has_name(arg$level1, arg$survey_weight))
  assertthat::assert_that(assertthat::has_name(arg$level1, arg$survey_id))
  if (length(arg$level2_modifiers) > 0
    || length(arg$level2_period1_modifiers) > 0) {
    if (!length(arg$level2) > 0) stop("modifier variables given without hierarchical ('level2') data")
    assertthat::assert_that(inherits(arg$level2, "data.frame"))
    assertthat::not_empty(arg$level2)
  }
  # If level2 exists, these variables must exist in it
  if (length(arg$level2) > 0) {
    assertthat::assert_that(has_all_names(arg$level2, c(arg$time_id,
          arg$geo_id, unlist(arg$level2_modifiers),
          unlist(arg$level2_period1_modifiers))))
  }
  if (length(arg$targets) > 0) {
    assertthat::assert_that(has_all_names(arg$targets,
        c(arg$time_id, arg$geo_id,
          unlist(arg$target_groups),
          unlist(arg$target_proportion))))
  }
  return(TRUE)
}

# Check argument types and throw an error if a check fails
check_arg_types <- function(arg) {
  if (!length(arg$level1) > 0) stop("no item ('level1') data")
  assertthat::assert_that(inherits(arg$level1, "data.frame"))
  assertthat::not_empty(arg$level1)
  assertthat::assert_that(is.numeric(arg$level1[[arg$time_id]]))
  if (!is.null(arg$level2)) {
    assertthat::assert_that(inherits(arg$level2, "data.frame"))
    assertthat::assert_that(all_strings(arg$level2_modifiers))
    assertthat::assert_that(all_strings(arg$level2_period1_modifiers))
  }
  if (!is.null(arg$targets)) {
    assertthat::assert_that(inherits(arg$targets, "data.frame"))
    assertthat::is.string(arg$target_proportion)
    assertthat::assert_that(all_strings(arg$target_groups))
  }
  assertthat::is.string(arg$items)
  assertthat::is.string(arg$time_id)
  assertthat::is.string(arg$groups)
  assertthat::is.string(arg$geo_id)
  assertthat::is.string(arg$survey_weight)
  assertthat::is.string(arg$survey_id)
  assertthat::is.count(arg$difficulty_count)
  assertthat::is.count(arg$min_surveys)
  assertthat::is.count(arg$min_periods)
  assertthat::is.count(arg$constant_item)
  assertthat::is.flag(arg$separate_periods)
  assertthat::is.flag(arg$silent)
  return(TRUE)
}

# Check argument ranges
check_arg_ranges <- function(arg) {
  assertthat::assert_that(is_positive(arg$delta_tbar_prior_sd))
  assertthat::assert_that(is_positive(arg$innov_sd_delta_scale))
  assertthat::assert_that(is_positive(arg$innov_sd_theta_scale))
  assertthat::assert_that(assertthat::is.count(arg$min_surveys))
  assertthat::assert_that(assertthat::is.count(arg$min_periods))
  return(TRUE)
}

set_arg_defaults <- function(arg) {
  if (is.null(arg$separate_periods)) arg$separate_periods <- FALSE
  if (is.null(arg$difficulty_count)) arg$difficulty_count <- 1L
  if (is.null(arg$min_surveys)) arg$min_surveys <- 1L
  if (is.null(arg$min_periods)) arg$min_periods <- 1L
  if (is.null(arg$constant_item)) arg$constant_item <- TRUE
  if (is.null(arg$silent)) arg$silent <- FALSE
  if (is.null(arg$delta_tbar_prior_mean)) arg$delta_tbar_prior_mean <- 0.5
  if (is.null(arg$delta_tbar_prior_sd)) arg$delta_tbar_prior_sd <- 0.5
  if (is.null(arg$innov_sd_delta_scale)) arg$innov_sd_delta_scale <- 2.5
  if (is.null(arg$innov_sd_theta_scale)) arg$innov_sd_theta_scale <- 2.5
  return(arg)
}
