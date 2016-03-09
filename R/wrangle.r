#' Format data for use with dgirt
#'
#' @param data Named `list` of `data.frame`s containing:
#'   \describe{
#'     \item{level1}{Survey responses at the lowest level of aggregation (e.g. individual).}
#'     \item{level2}{Survey responses at higher level
#'        of aggregation. Currently restricted to geographic units, and must be
#'        accompanied by `level2_modifiers`.}
#'     \item{targets}{Population targets for stratification. Strata in
#'        `level1` must be a subset of those in the target data,
#'        and there can't be missingness in the stratifying variables.}
#'   }
#' @param vars Named `list` of character vectors that give the variable or variables in representing:
#'   \describe{
#'     \item{items}{Item responses in `data$level1`}
#'     \item{groups}{Respondent characteristics in `data$level1`.}
#'     \item{time_id}{Time period in `data$level1` and `data$level2`.}
#'     \item{geo_id}{Geographic identifier in `data$level1` and `data$level2`.}
#'     \item{survey_id}{Survey identifier in `data$level1`.}
#'     \item{survey_weight}{Survey weight in `data$level1`.}
#'     \item{target_groups}{Respondent characteristics in `data$targets`.}
#'     \item{target_proportion}{Population proportions in `data$targets`.}
#'     \item{level2_modifiers}{Geographic characteristics in `data$level2`.}
#'     \item{level2_period1_modifiers}{Geographic characteristics in `data$level2` to be used in place of `vars$level2_modifiers` in the first period.}
#'   }
#' @param filters Named `list` of filters that will be applied to the data.
#'   \describe{
#'     \item{periods}{A numeric vector of time periods to be used in estimation,
#'        including unobserved periods or excluding observed periods as desired.
#'        Defaults to the values of the `vars$time_id` variable in the data.}
#'     \item{geo_ids}{A character vector giving the subset of values of `vars$geo_id` in the data to be used in estimation.
#'        Defaults to the values of `vars$geo_id` in the data.}
#'     \item{min_surveys}{A positive integer giving the minimum surveys in which an item must appear to be used in estimation.}
#'     \item{min_periods}{A positive integer giving the minimum periods in which an item must appear to be used in esimation.}
#'   }
#' @param params Named `list` of modeling choices.
#'   \describe{
#'     \item{separate_periods}{Logical for whether estimates should be pooled over time. No pooling if `TRUE`.}
#'     \item{constant_item}{Logical for whether item parameters should be constant over time.}
#'     \item{delta_tbar_prior_mean}{Prior mean of $\\bar{\\delta_t}$.}
#'     \item{delta_tbar_prior_sd}{Prior standard deviation of $\\bar{\\delta_t}$.}
#'     \item{innov_sd_delta_scale}{Prior scale of innovation parameter for standard deviation of $\\bar{\\delta_t}$.}
#'     \item{innov_sd_theta_scale}{Prior scale of innovation for standard deviation of group ability parameter $\\bar{\\theta_t}$.}
#'   }
#' @return \code{list} List formatted for `dgirt`.
#' @export

# data(state_opinion)
# data = list(level1 = state_opinion)
# vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
#             groups = c("race"),
#             time_id = "year",
#             geo_id = "state",
#             survey_id = "source",
#             survey_weight = "weight")
# filters = list(periods = c(2006:2010))

data(state_opinion)
data = list(level1 = state_opinion,level2 = dplyr::mutate(state_opinion, education = sample(1:2, nrow(state_opinion), replace = TRUE)) %>% dplyr::distinct(state, year))
vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
            groups = c("race"),
            time_id = "year",
            geo_id = "state",
            survey_id = "source",
            survey_weight = "weight",
            level2_modifiers = "education",
            level2_period1_modifiers = "education")
filters = list(periods = c(2006:2010))

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
  item$modifier <- Modifier$new()
  item$filters <- Filter$new()
  item$targets <- Target$new()
  item$control <- Control$new()

  item$tbl <- arg$level1
  # item$tbl <- data.frame(a = 1)

  item$items <- new("ItemVar", arg$items)
  item$geo <- new("ItemVar", arg$geo_id)
  item$time <- new("ItemVar", arg$time_id)
  item$survey <- new("ItemVar", arg$survey_id)
  item$targets$weight <- new("ItemVar", arg$survey_weight)

  if (length(arg$level2) > 0) {
    item$modifier$tbl <- arg$level2
    item$modifier$modifiers <- new("ItemVar", arg$level2_modifiers)
    item$modifier$t1_modifiers <- new("ItemVar", arg$level2_period1_modifiers)
    item$modifier$time <- new("ItemVar", item$time)
    item$modifier$geo <- new("ItemVar", item$geo)
  }

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
