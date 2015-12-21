handle_arguments <- function() {
  arg <- mget(names(formals(wrangle)), parent.frame(),
    ifnotfound = list(rep(NULL, length(formals(wrangle)))))
  arg <- unlist(arg, recursive = FALSE)
  assertthat::not_empty(names(arg))
  names(arg) <- sub("^(data|vars|filters|params)\\.", "", names(arg))
  check_arg_lengths(arg)
  check_arg_names(arg)
  check_arg_types(arg)
  check_arg_ranges(arg)
  arg$min_periods <- as.integer(arg$min_periods)
  arg$min_surveys <- as.integer(arg$min_surveys)
  return(arg)
}

check_arg_lengths <- function(.arg) {
  assertthat::not_empty(.arg$items)
  assertthat::is.string(.arg$time_id)
  assertthat::is.string(.arg$geo_id)
  assertthat::is.string(.arg$survey_weight)
  assertthat::is.string(.arg$survey_id)
  if (length(.arg$target_proportion) > 0) {
    assertthat::is.string(.arg$target_proportion)
  }
  return(TRUE)
}

# Check argument ranges
check_arg_ranges <- function(.arg) {
  assertthat::assert_that(is_positive(.arg$delta_tbar_prior_sd))
  assertthat::assert_that(is_positive(.arg$innov_sd_delta_scale))
  assertthat::assert_that(is_positive(.arg$innov_sd_theta_scale))
  assertthat::assert_that(assertthat::is.count(.arg$min_surveys))
  assertthat::assert_that(assertthat::is.count(.arg$min_periods))
  return(TRUE)
}

# Check that names given in arguments appear in data.frame arguments
check_arg_names <- function(..arg) {
  assertthat::assert_that(has_all_names(..arg$level1, ..arg$items))
  assertthat::assert_that(has_all_names(..arg$level1, ..arg$groups))
  assertthat::assert_that(assertthat::has_name(..arg$level1, ..arg$time_id))
  assertthat::assert_that(assertthat::has_name(..arg$level1, ..arg$geo_id))
  assertthat::assert_that(assertthat::has_name(..arg$level1, ..arg$survey_weight))
  assertthat::assert_that(assertthat::has_name(..arg$level1, ..arg$survey_id))
  if (length(..arg$level2_modifiers) > 0
    || length(..arg$level2_period1_modifiers) > 0) {
    assertthat::assert_that(inherits(..arg$level2, "data.frame"))
    assertthat::not_empty(..arg$level2)
  }
  # If level2 exists, these variables must exist in it
  if (length(..arg$level2) > 0) {
    assertthat::assert_that(has_all_names(..arg$level2, c(..arg$time_id,
          ..arg$geo_id, unlist(..arg$level2_modifiers),
          unlist(..arg$level2_period1_modifiers))))
  }
  if (length(..arg$targets) > 0) {
    assertthat::assert_that(has_all_names(..arg$level2,
        c(..arg$time_id, unlist(..arg$groups), ..arg$geo_id,
          unlist(..arg$target_groups),
          unlist(..arg$target_proportion))))
  }
  return(TRUE)
}

  # Check argument types and throw an error if a check fails
check_arg_types <- function(..arg) {
  assertthat::assert_that(inherits(..arg$level1, "data.frame"))
  assertthat::not_empty(..arg$level1)
  if (!is.null(..arg$level2)) {
    assertthat::assert_that(inherits(..arg$level2, "data.frame"))
    assertthat::assert_that(all_valid_strings(..arg$level2_modifiers))
    assertthat::assert_that(all_valid_strings(..arg$level2_period1_modifiers))
  }
  if (!is.null(..arg$targets)) {
    assertthat::assert_that(inherits(..arg$targets, "data.frame"))
    assertthat::is.string(..arg$target_proportion)
    assertthat::assert_that(all_valid_strings(..arg$target_groups))
  }
  assertthat::is.string(..arg$items)
  assertthat::is.string(..arg$time_id)
  assertthat::is.string(..arg$groups)
  assertthat::is.string(..arg$geo_id)
  assertthat::is.string(..arg$survey_weight)
  assertthat::is.string(..arg$survey_id)
  assertthat::is.count(..arg$difficulty_count)
  assertthat::is.count(..arg$min_surveys)
  assertthat::is.count(..arg$min_periods)
  assertthat::is.count(..arg$constant_item)
  assertthat::is.flag(..arg$separate_periods)
  assertthat::is.flag(..arg$silent)
  return(TRUE)
}

set_arg_defaults <- function(..arg) {
  if (is.null(..arg$separate_periods))
    ..arg$separate_periods <- FALSE
  if (is.null(..arg$difficulty_count))
    ..arg$difficulty_count <- 1L
  if (is.null(..arg$min_surveys))
    ..arg$min_surveys <- 1L
  if (is.null(..arg$min_periods))
    ..arg$min_periods <- 1L
  if (is.null(..arg$constant_item))
    ..arg$constant_item <- TRUE
  if (is.null(..arg$silent))
    ..arg$silent <- FALSE
  if (is.null(..arg$delta_tbar_prior_mean))
    ..arg$delta_tbar_prior_mean <- 0.5
  if (is.null(..arg$delta_tbar_prior_sd))
    ..arg$delta_tbar_prior_sd <- 0.5
  if (is.null(..arg$innov_sd_delta_scale))
    ..arg$innov_sd_delta_scale <- 2.5
  if (is.null(..arg$innov_sd_theta_scale))
    ..arg$innov_sd_theta_scale <- 2.5
  return(..arg)
}
