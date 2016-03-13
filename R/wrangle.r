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

wrangle <- function(data = list(level1,
                                level2 = NULL,
                                targets = NULL,
                                aggregates = NULL),
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

  arg <- handle_arguments()
  level1 <- handle_data(.data = arg$level1,
    covariates = c(arg$time_id, arg$geo_id, arg$groups, arg$survey_id),
    factorize = TRUE,
    .arg = arg)
  if (length(arg$level2) > 0) {
    arg$level2 <- handle_data(.data = arg$level2,
      covariates = unique(c(arg$time_id, arg$geo_id, arg$level2_modifiers,
          arg$level2_period1_modifiers)),
      # TODO: handle
      factorize = FALSE,
      .arg = arg)
  }

  ## INDIVIDUAL LEVEL ##

  # Filter data
  checks <- list()
  # while (!length(checks) > 0 || !all(checks))
  checks <- check_restrictions(level1, arg)
  # Find rows with no valid question responses
  level1$none_valid <- get_missing_respondents(level1[, arg$items, drop = FALSE])
  level1 <- apply_restrictions(level1, checks, arg)
  # Update arg$items
  arg$items <- intersect(arg$items, names(level1))
  arg$use_t <- set_use_t(level1, arg)
  arg$use_geo <- update_use_geo(level1, arg)
  level1 <- subset_to_estimation_periods(level1, arg)
  if (length(arg$level2) > 0) {
    arg$level2 <- subset_to_estimation_periods(arg$level2, arg)
    time_diff <- setdiff(
        unlist(dplyr::select_(level1, arg$time_id)),
        unlist(dplyr::select_(arg$level2, arg$time_id)))
    if (length(time_diff) > 0) {
      stop("missing hierarchical data in ", paste(time_diff, collapse = ", "))
    }
  }
  # TODO: instead of reapplying once here, should loop over restrictions until no changes
  checks <- check_restrictions(level1, arg)
  level1 <- apply_restrictions(level1, checks, arg)
  arg$items <- intersect(arg$items, names(level1))
  checks <- check_restrictions(level1, arg)
  level1 <- apply_restrictions(level1, checks, arg)
  arg$items <- intersect(arg$items, names(level1))

  # Create weights from population targets
  if (length(arg$targets) > 0) {
    level1 <- create_weights(level1, arg)
  }

  level1 <- add_gt_variables(level1, arg)
  level1 <- drop_rows_missing_items(level1, arg)

  # Fix factor levels after filtering
  level1 <- droplevels(level1)
  arg$use_geo <- update_use_geo(level1, arg)
  arg$level2 <- subset_to_observed_geo_periods(arg$level2, arg)
  nonmissing_t <- sort(unique(level1[[arg$time_id]]))

  group_grid <- make_group_grid(level1, arg$groups, arg) %>%
    dplyr::arrange_(.dots = c(arg$time_id, arg$groups, arg$geo_id))
  group_grid_t <- group_grid %>%
    dplyr::select_(lazyeval::interp(~-one_of(v), v = arg$time_id)) %>%
    dplyr::distinct() %>%
    dplyr::arrange_(.dots = c(arg$groups, arg$geo_id))

  group_levels <- vapply(arg$groups, function(i) length(levels(group_grid_t[[i]])), numeric(1))
  if (any(group_levels < 2)) {
    stop("no variation in group variable: ", paste(names(which(group_levels < 2)), collapse = ", "))
  }

  group_counts <- make_group_counts(level1, group_grid, arg)

  if (length(data$aggregates) > 0) {
    aggregates <- data$aggregates %>%
      dplyr::filter_(lazyeval::interp(~geo %in% arg$use_geo |
        geo %in% paste0(arg$geo_id, arg$use_geo), geo = as.name(arg$geo_id)))

    gss_group_grid = make_group_grid(aggregates, arg$groups, arg) %>%
      dplyr::arrange_(.dots = c(arg$time_id, arg$groups, arg$geo_id))
    gss_group_grid_t <- gss_group_grid %>%
      dplyr::select_(lazyeval::interp(~-one_of(v), v = arg$time_id)) %>%
      dplyr::distinct() %>%
      dplyr::arrange_(.dots = c(arg$groups, arg$geo_id))

    gss_group_levels <- vapply(arg$groups, function(i) length(levels(gss_group_grid_t[[i]])), numeric(1))
    if (any(group_levels < 2)) {
      stop("no variation in group variable: ", paste(names(which(group_levels < 2)), collapse = ", "))
    }

    aggregates <- factorize_arg_vars(aggregates, arg)
    gss_group_counts = aggregates %>% 
      mutate(name = paste(D_year, item, paste0(D_abb, "__", D_black), sep = " | ")) %>%
        select(name, everything())

    group_grid <- suppressWarnings(bind_rows(group_grid, gss_group_grid)) %>%
      dplyr::arrange_(.dots = c(arg$time_id, arg$groups, arg$geo_id))
    # FIXME: bind_rows coerces factors with unequal factor levels back to character
    group_grid <- factorize_arg_vars(group_grid, arg)
    group_grid_t <- group_grid %>%
      dplyr::select_(lazyeval::interp(~-one_of(v), v = arg$time_id)) %>%
      dplyr::distinct() %>%
      dplyr::arrange_(.dots = c(arg$groups, arg$geo_id))

    group_counts <- suppressWarnings(bind_rows(group_counts, gss_group_counts)) %>%
      # group counts must exclude unobserved groups
      dplyr::filter_(~n_grp > 0) %>%
      dplyr::arrange_(.dots = c(arg$time_id, "item", arg$groups, arg$geo_id))
    group_counts <- factorize_arg_vars(group_counts, arg) %>%
      dplyr::arrange_(.dots = c(arg$time_id, "item", arg$groups, arg$geo_id))

    aggregate_items <- unique(as.character(aggregates$item))
    message("Added ", length(setdiff(aggregate_items, arg$items)), " new items from aggregate data.")

    arg$items <- sort(unique(c(arg$items, aggregate_items)))

  }

  MMM <- create_missingness_array(group_counts, group_grid, arg)

  T <- length(arg$use_t)
  Q <- length(unique(group_counts$item))
  G <- count_covariate_combos(group_counts, arg)

  # Create placeholders
  # TODO: confirm that count_level2_groups works with > 1 group
  Gl2 <- count_level2_groups(arg$level2, G, arg)
  WT <- make_dummy_weight_matrix(T, Gl2, G)
  l2_only <- make_dummy_l2_only(unique(group_counts$item), arg)
  NNl2 <- make_dummy_l2_counts(unique(group_counts$item), T, Q, Gl2 = Gl2, arg$level2_modifiers, arg)
  SSl2 <- make_dummy_l2_counts(unique(group_counts$item), T, Q, Gl2 = Gl2, arg$level2_modifiers, arg)

  group_design_matrix <- make_design_matrix(group_grid_t, arg$groups, arg)
  ZZ <- make_hierarchical_array(level1, arg$level2, arg$level2_modifiers, group_design_matrix, group_grid_t, arg) 
  ZZ_prior <- ZZ

  stan_data <- list(
    n_vec = setNames(group_counts$n_grp, group_counts$name),
    s_vec = setNames(group_counts$s_grp, group_counts$name),
    NNl2 = NNl2,
    SSl2 = SSl2,
    XX = group_design_matrix,
    ZZ = ZZ,            # geographic predictors
    ZZ_prior = ZZ_prior,
    MMM = MMM,          # missingness array (T x Q x G)
    G = G,              # number of covariate groups
    Q = Q,              # number of questions (items)
    T = T,              # number of time units (years)
    N = nrow(group_counts),  # number of observed group-question cells
    P = ncol(group_design_matrix),       # number of hierarchical parameters
    S = dim(ZZ)[[2]],   # number of geographic units
    H = dim(ZZ)[[3]],   # number of geographic-level predictors
    Hprior = dim(ZZ_prior)[[3]],
    separate_t = as.integer(arg$separate_periods),  # if 1, no pooling over time
    constant_item = as.integer(arg$constant_item),  # if 1, item parameters constant
    D = ifelse(arg$constant_item, 1L, T),           # number of difficulty parameters
    WT = WT,            # weight matrix for calculating level-two mean
    l2_only = l2_only,
    Gl2 = Gl2,          # number of second-level groups
    delta_tbar_prior_mean = arg$delta_tbar_prior_mean,
    delta_tbar_prior_sd = arg$delta_tbar_prior_sd,
    innov_sd_delta_scale = arg$innov_sd_delta_scale,
    innov_sd_theta_scale = arg$innov_sd_theta_scale,
    group_counts = group_counts,
    vars = list(items = arg$items,
                gt_items = sort(unique(group_counts$item)),
                groups = arg$groups,
                time_id = arg$time_id,
                use_t = arg$use_t,
                geo_id = arg$geo_id,
                periods = arg$periods,
                survey_id = arg$survey_id,
                covariate_groups = group_grid_t,
                hier_names = dimnames(group_design_matrix)[[2]]))

  # Check dimensions against what Stan will expect
  check_dimensions(stan_data)
  check_values(stan_data)
  check_order(stan_data)

  return(stan_data)
}

make_hierarchical_array <- function(level1, level2, level2_modifiers, group_design_matrix, group_grid_t, arg) {
  if (length(level2) > 0) {
    ZZ <- shape_hierarchical_data(level2, level2_modifiers, group_grid_t, arg)
  } else {
    zz.names <- list(arg$use_t, dimnames(group_design_matrix)[[2]], "")
    ZZ <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  }
  ZZ
}

shape_hierarchical_data <- function(level2, modifiers, group_grid_t, arg) {
  # the array of hierarchical data ZZ should be T x P x H, where T is the number of time periods, P is the number of
  # hierarchical parameters (including the geographic), and H is the number of predictors for geographic unit effects
  # TODO: make flexible; as written we must model geo x t
  modeled_params = c(arg$geo_id, arg$time_id)
  unmodeled_params = setdiff(c(arg$geo_id, arg$time_id, arg$groups), modeled_params)
  # TODO: uniqueness checks on level2 data (state_demographics should not pass)
  # TODO: confirm sort order of level2_modifiers
  missing_level2_geo <- setdiff(
    unique(unlist(dplyr::select_(group_grid_t, arg$geo_id))),
    unique(unlist(dplyr::select_(level2, arg$geo_id))))
  if (length(missing_level2_geo) > 0) stop("no hierarchical data for geo in item data: ", missing_level2_geo)
  missing_level2_t <- setdiff(arg$use_t, unlist(dplyr::select_(level2, arg$time_id)))
  if (length(missing_level2_t) > 0) stop("missing hierarchical data for t in item data: ", missing_level2_t)

  hier_frame <- level2 %>% dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~paste0(arg$geo_id, geo),
      geo = as.name(arg$geo_id))), arg$geo_id)) %>%
    dplyr::select_(.dots = c(modeled_params, arg$level2_modifiers)) %>%
    dplyr::rename_("param" = arg$geo_id) %>%
    dplyr::mutate_("param" = ~as.character(param)) %>%
    dplyr::arrange_(.dots = c("param", arg$time_id))

  modeled_param_names <- unique(unlist(dplyr::select_(hier_frame, "param")))
  unmodeled_param_levels = lapply(unmodeled_params, function(x) {
      paste0(x, levels(group_grid_t[[x]]))[-1]
    }) %>% unlist()
  param_levels <- c(modeled_param_names, unmodeled_param_levels)

  unmodeled_frame <- expand.grid(c(list(
      unmodeled_param_levels, 
      sort(unique(unlist(dplyr::select_(level2, arg$time_id))))),
      rep(list(0), length(modifiers)))) %>%
    setNames(c("param", arg$time_id, modifiers)) %>%
    dplyr::arrange_(.dots = c("param", arg$time_id))

  unmodeled_factors <- hier_frame %>%
    dplyr::select_(~-one_of("param")) %>%
    dplyr::summarise_each(~class(.) %in% c("character", "factor")) %>%
    unlist()
  unmodeled_factors <- names(unmodeled_factors)[which(unmodeled_factors)]
  if (length(unmodeled_factors) > 0) {
    unmodeled_frame <- unmodeled_frame %>%
      dplyr::mutate_each_(~as.character, vars = unmodeled_factors)
  }

  hier_frame <- dplyr::bind_rows(hier_frame, unmodeled_frame) %>%
    dplyr::mutate_each_(~ifelse(is.na(.), 0, .), vars = arg$level2_modifiers)
  hier_melt <- wrap_melt(hier_frame, id.vars = c("param", arg$time_id), variable.name = "modifiers") %>%
    dplyr::mutate_("param" = ~factor(param, levels = param_levels, ordered = TRUE))
  if (!inherits(hier_melt$value, "numeric")) stop("non-numeric values in hierarchical data. Factor handling probably failed. Possible quickfix: omit or manually dummy out any factors in 'level2_modifiers' or 'level2_period1_modifiers'.")
  assertthat::assert_that(names_subset(modeled_param_names, unlist(hier_melt$param)))
  assertthat::assert_that(names_subset(unmodeled_param_levels, unlist(hier_melt$param)))
  melt_formula <- as.formula(paste(arg$time_id, "param", "modifiers", sep = " ~ "))
  zz <- reshape2::acast(hier_melt, melt_formula, drop = FALSE, value.var = "value")
  zz <- zz[, -1, , drop = FALSE]
  zz
}

add_gt_variables <- function(level1, arg) {
  gt_table <- create_gt_variables(d = level1, .items = arg$items)
  level1 <- dplyr::bind_cols(level1, gt_table)
  return(level1)
}


make_group_counts <- function(level1, group_grid, arg) {
  level1$n_responses <- count_respondent_trials(level1)
  group_design_effects <- compute_group_design_effects(level1, arg)
  group_trial_counts <- count_group_trials(level1, group_design_effects,
    group_grid, arg)
  mean_group_outcome <- compute_mean_group_outcome(level1, group_grid, arg)
  group_success_counts <- count_group_successes(group_trial_counts,
    mean_group_outcome, arg)
  format_counts(group_trial_counts, group_success_counts, arg)
}

create_missingness_array <- function(group_counts, group_grid, arg) {
  missingness <- get_missingness(group_counts, group_grid, arg)
  cast_missingness(missingness, arg)
}

as_tbl <- function(x) {
  # assertthat::assert_that(not_empty(x), is.data.frame(x))
  assertthat::assert_that(not_empty(x), is.data.frame(x))
  dplyr::as.tbl(x)
}

check_dimensions <- function(d) {
  assertthat::assert_that(equal_length(d$n_vec, d$s_vec))
  assertthat::assert_that(all_equal(dim(d$NNl2), as.integer(c(d$T, d$Q, d$Gl2))))
  assertthat::assert_that(all_equal(dim(d$SSl2), as.integer(c(d$T, d$Q, d$Gl2))))
  assertthat::assert_that(all_equal(dim(d$MMM), c(d$T, d$Q, d$G)))
  assertthat::assert_that(all_equal(dim(d$WT), as.integer(c(d$T, d$Gl2, d$G))))
  assertthat::assert_that(all_equal(dim(d$l2_only), c(d$T, d$Q)))
  assertthat::assert_that(all_equal(dim(d$XX), c(d$G, d$P)))
  assertthat::assert_that(all_equal(dim(d$ZZ), c(d$T, d$P, d$H)))
  assertthat::assert_that(all_equal(dim(d$ZZ_prior), c(d$T, d$P, d$H)))
  assertthat::assert_that(not_empty((d$constant_item)))
  assertthat::assert_that(not_empty((d$separate_t)))
}

check_restrictions <- function(level1, .arg) {
  checks <- list()
  # # We need to find out which time periods are observed in the data
  # checks$observed_periods <- get_observed_t(level1[[.arg$time_id]])
  #
  # Which item columns have no valid responses?
  checks$q_all_missing <- get_missing_items(level1, .arg$items)

  # Which variables don't satisfy the min_periods requirement?
  checks$q_when_asked <- get_question_periods(level1, .arg)
  checks$q_rare.t <- get_rare_items_over_t(checks, .arg)

  # Which variables don't satisfy the min_surveys requirement?
  checks$q_which_asked <- get_question_polls(level1, .arg)
  checks$q_rare_polls <- get_rare_items_over_polls(checks, .arg)

  return(checks)
}

get_question_periods <- function(.data, .arg) {
  .data %>%
    dplyr::group_by_(.arg$time_id) %>%
    dplyr::summarise_each_(~anyValid, vars = .arg$items)
}

# checks <- list(q_when_asked = get_question_periods(d, a))
# .arg = list(time_id = "t", min_periods = 1L)
# get_rare_items_over_t(checks, .arg)

get_rare_items_over_t <- function(.checks, .arg) {
  assertthat::assert_that(is_count(.arg$min_periods))
  q_t_count <- colSums(dplyr::select_(.checks$q_when_asked,
    lazyeval::interp(~-one_of(v), v = .arg$time_id)))
  q_rare <- names(q_t_count)[q_t_count < .arg$min_periods]
  return(q_rare)
}

get_question_polls <- function(.data, .arg) {
  .data %>%
    dplyr::group_by_(.arg$survey_id) %>%
    dplyr::summarise_each_(dplyr::funs(anyValid), vars = .arg$items)
}

get_rare_items_over_polls <- function(.checks, ..arg) {
  q_counts <- .checks$q_which_asked %>%
    dplyr::select_(lazyeval::interp(~-one_of(v),
      v = ..arg$survey_id)) %>%
    dplyr::summarise_each(~sum)
  lt_min_surveys <- colnames(q_counts)[unlist(q_counts) < ..arg$min_surveys]
  lt_min_surveys
}

get_observed_t <- function(t_data) {
  observed_t <- unlist(t_data)
  assertthat::assert_that(is.numeric(observed_t), not_empty(observed_t))
  unique(observed_t)
}

get_missing_items <- function(.data, item_names) {
  nonmissing <- .data %>%
    dplyr::select_(lazyeval::interp(~one_of(v), v = item_names)) %>%
    dplyr::summarise_each(~sum(!is.na(.)) > 0) %>%
    wrap_melt(id.vars = NULL) %>%
    dplyr::filter_(~value == FALSE)
  unlist(nonmissing$variable)
}

get_missing_respondents <- function(item_data) {
  not_na <- !is.na(item_data)
  rowSums(not_na) == 0
}

compute_group_design_effects <- function(.data, .arg) {
  assertthat::assert_that(is_numeric(.data[[.arg$survey_weight]]))
  assertthat::assert_that(all_strings(c(.arg$survey_weight, .arg$geo_id, .arg$time_id)), all_strings(.arg$groups))
  assertthat::assert_that(has_all_names(.data, c(.arg$survey_weight, .arg$geo_id, .arg$time_id)),
    has_all_names(.data, .arg$groups))

  de_table <- dplyr::as.tbl(.data) %>%
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
    dplyr::select_(.arg$survey_weight) %>%
    dplyr::summarise_("def" = lazyeval::interp(~create_design_effects(w),
  w = as.name(.arg$survey_weight))) %>%
    dplyr::arrange_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id))

  assertthat::assert_that(has_all_names(de_table, c("def", .arg$geo_id, .arg$groups, .arg$time_id)))
  return(de_table)
}

count_group_trials <- function(.data, design_effects, group_grid, .arg) {
  assertthat::assert_that(not_empty(.data), length(grep("_gt\\d+$", colnames(.data))) > 0)
  not_na_trial <- .data %>%
    # The _gt variables can take values of 0/1/NA
    dplyr::mutate_each_(~notNA, ~matches("_gt\\d+$"))
    # For a respondent who only answered questions A and B, we now have
    #  T,   T,   T,   T,   T,   F,   F.
  assertthat::assert_that(not_empty(not_na_trial))
  trial_counts <- not_na_trial %>%
    dplyr::mutate_each_(~. / n_responses, vars = ~matches("_gt\\d+$")) %>%
    # Dividing the booleans by n_responses = 5 calculated earlier gives us
    #.02, .02, .02, .02, .02,   0,   0.
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
    dplyr::summarise_each_(~sum, vars = ~matches("_gt\\d+$")) %>%
    dplyr::ungroup() %>%
    # Summing over respondents within the grouping variables gives the count
    # of trials for each combination of geo, demo, and time variables, divided
    # equally across the _gt indicators.
    # Joining the design effect table by the grouping variables attaches
    # the design effect associated with each variable combination.
    muffle_full_join(design_effects, by = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
    dplyr::mutate_each_(~ceiling(. / def), vars = ~matches("_gt\\d+$")) %>%
    # Dividing by the design effect gives us a design-effect-adjusted trial
    # count.
    dplyr::select_(~-def)# %>%
    # Tidy up.
    # dplyr::mutate_each_(~as.character, vars = c(.arg$geo_id, .arg$groups, .arg$time_id))

  # A dplyr::full_join of trial_counts and group_grid will create rows for the desired
  # covariate combinations, where unobserved cells take NA values
  trial_counts <- muffle_full_join(trial_counts, group_grid,
    by = c(.arg$geo_id, .arg$groups, .arg$time_id))
  # Replace the NA values in those rows representing unobserved covariate
  # combinations with zeroes
  trial_counts <- trial_counts %>% dplyr::mutate_each_(~replaceNA, ~matches("_gt\\d+$"))
  # NOTE: Order will matter in a moment
  trial_counts <- trial_counts %>%
    dplyr::arrange_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
    dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  return(trial_counts)
}

compute_mean_group_outcome <- function(level1, group_grid, .arg) {
  mean_y <- level1 %>%
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
    # subset to identifiers and items
    dplyr::select_(~matches("_gt\\d+$"), ~n_responses, .arg$survey_weight) %>%
    # weight by n_responses
    dplyr::mutate_("weight" = lazyeval::interp(~ w / n,
      w = as.name(.arg$survey_weight), n = quote(n_responses))) %>%
    # take weighted mean of item responses within geo, group, time
    dplyr::summarise_each_(~weighted.mean(as.vector(.), weight,
      na.rm = TRUE), vars = "contains(\"_gt\")") %>%
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
    # replace NaN
    dplyr::mutate_each(~replaceNaN) %>%
    dplyr::ungroup()

  # make sure missing group appear as NA
  mean_y <- muffle_full_join(mean_y, group_grid,
    by = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
      # NOTE: Order will matter in a moment
      dplyr::arrange_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id)) %>%
      dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  return(mean_y)
}

count_group_successes <- function(trial_counts, mean_y, .arg) {
  # Confirm row order is identical before taking product
  assertthat::assert_that(all_equal(
      dplyr::select_(mean_y, .dots = c(.arg$geo_id, .arg$groups, .arg$time_id)),
      dplyr::select_(trial_counts, .dots = c(.arg$geo_id, .arg$groups, .arg$time_id))))
  success_counts <- get_gt(trial_counts) * get_gt(mean_y)
  success_counts <- success_counts %>%
    # Reattach our identifiers
    dplyr::bind_cols(dplyr::select_(trial_counts, .dots = c(.arg$geo_id, .arg$groups, .arg$time_id)), .) %>%
    # Round off returning integers and replace NA with 0
    dplyr::ungroup() %>%
    dplyr::mutate_each_(~replaceNA, ~matches("_gt\\d+$")) %>%
    dplyr::mutate_each_(~round(., digits = 0), ~matches("_gt\\d+$")) %>%
    dplyr::arrange_(.dots = c(.arg$geo_id, .arg$groups, .arg$time_id))
  return(success_counts)
}

make_design_matrix <- function(group_grid_t, factors, arg) {
  design_formula <- as.formula(paste("~ 0", arg$geo_id, paste(factors, collapse = " + "), sep = " + "))
  design_matrix <- with_contr.treatment(model.matrix(design_formula, group_grid_t))
  design_matrix <- cbind(design_matrix, group_grid_t) %>%
    dplyr::arrange_(.dots = c(factors, arg$geo_id))
  group_names <- concat_groups(group_grid_t, factors, arg$geo_id, "name")
  design_matrix <- concat_groups(design_matrix, factors, arg$geo_id, "name")
  assertthat::assert_that(all_equal(group_names$name, design_matrix$name))
  rownames(design_matrix) <- design_matrix$name
  design_matrix <- design_matrix %>% dplyr::select_(~-one_of("name")) %>%
    as.matrix()
  # We have an indicator for each geographic unit; drop one
  design_matrix <- design_matrix[, -1, drop = FALSE]
  assertthat::assert_that(identical(rownames(design_matrix), group_names$name))
  invalid_values <- setdiff(as.vector(design_matrix), c(0, 1))
  if (length(invalid_values) > 0) {
    stop("design matrix values should be in (0, 1); found ", paste(sort(invalid_values), collapse = ", "))
  }
  return(design_matrix)
}

concat_groups <- function(tabular, group_names, geo_id, name) {
  has_all_names(tabular, group_names)
  has_all_names(tabular, geo_id)
  tabular %>%
    tidyr::unite_("group_concat", group_names, sep = "_") %>%
    tidyr::unite_(name, c("group_concat", geo_id), sep = "_x_")
}

split_groups <- function(tabular, group_names, geo_id, name) {
  assertthat::assert_that(has_name(tabular, "name"))
  tabular %>%
    tidyr::separate_(name, c("group_concat", geo_id), sep = "_x_") %>%
    tidyr::separate_("group_concat", group_names, sep = "_")
}
# split_groups(concat_groups(group_grid_t, arg$groups, arg$geo_id, "name"), arg$groups, arg$geo_id, "name")

summarize_trials_by_period <- function(trial_counts, .arg) {
  dplyr::select_(trial_counts, ~matches("_gt\\d+$"), .arg$time_id) %>%
    reshape2::melt(id.vars = .arg$time_id) %>%
    dplyr::group_by_(.arg$time_id) %>%
    dplyr::summarise_(valid_items = ~sum(value, na.rm = TRUE) > 0)
}

format_counts <- function(trial_counts, success_counts, .arg) {
  trial_counts_melt <- wrap_melt(trial_counts, variable.name = "item",
    id.vars = c(.arg$geo_id, .arg$groups, .arg$time_id), value.name = "n_grp")
  success_counts_melt <- wrap_melt(success_counts, variable.name = "item",
    id.vars = c(.arg$geo_id, .arg$groups, .arg$time_id), value.name = "s_grp")
  joined <- dplyr::left_join(trial_counts_melt, success_counts_melt,
      by = c(.arg$geo_id, .arg$groups, .arg$time_id, "item")) %>%
    tidyr::unite_("name", c(.arg$geo_id, .arg$groups), sep = "__", remove = FALSE) %>%
    dplyr::filter_(~n_grp != 0) %>%
    dplyr::mutate_(
      n_grp = ~replaceNA(n_grp),
      s_grp = ~replaceNA(s_grp),
      name = lazyeval::interp(~paste(period, item, name, sep = " | "), period = as.name(.arg$time_id))) %>%
      # NOTE: arrange by time, item, group. Otherwise dgirt() output won't have
      # the expected order.
    dplyr::arrange_(.dots = c(.arg$time_id, "item", .arg$groups, .arg$geo_id))
  return(joined)
}

wrap_melt <- function(...) {
  melt <- reshape2::melt(...)
  melt_args <- list(...)
  # make the "variable" variable, whatever it's called, a character vector
  # instead of factor
  if (length(melt_args$variable.name) > 0) {
    melt[[melt_args$variable.name]] <- as.character(melt[[melt_args$variable.name]])
  } else {
    melt$variable <- as.character(melt$variable)
  }
  return(melt)
}

get_missingness <- function(group_counts, group_grid, arg) {
  group_counts %>%
    # Include in the missingness array all group-variables that might not exist
    # in the data because of unobserved use_t
    muffle_full_join(group_grid, by = c(arg$geo_id, arg$groups, arg$time_id)) %>%
    # Get missingness by group
    dplyr::group_by_(.dots = c("item", arg$time_id, arg$groups, arg$geo_id)) %>%
    dplyr::summarise_("m_grp" = ~as.integer(sum(n_grp, na.rm = TRUE) == 0)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(.dots = c(arg$time_id, "item", arg$groups, arg$geo_id))
}

cast_missingness <- function(missingness, arg) {
  acast_formula <- as.formula(paste0(arg$time_id, "~ item ~", paste(arg$groups, collapse = "+"), "+", arg$geo_id))
  MMM <- missingness %>%
    dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~paste0("x_", geo), geo = as.name(arg$geo_id))), arg$geo_id)) %>%
    dplyr::arrange_(.dots = c(arg$time_id, "item", arg$groups, arg$geo_id)) %>%
    reshape2::acast(acast_formula, value.var = "m_grp", fill = 1)
  # But we don"t want to include the character string "NA" as a variable
  MMM <- MMM[
    !(dimnames(MMM)[[1]] == "NA"),
    !(dimnames(MMM)[[2]] == "NA"),
    !(dimnames(MMM)[[3]] == "NA"), drop = FALSE]
  # No cells should be NA either
  assertthat::assert_that(all_in(MMM, c(0, 1)))
  return(MMM)
}

make_dummy_weight_matrix <- function(T, Gl2, G) {
  array(1, dim = c(T, Gl2, G))
}

make_dummy_l2_only <- function(items, arg) {
  l2_only <- matrix(0L,
    nrow = length(arg$use_t),
    ncol = length(items),
    dimnames = list(arg$use_t, items))
  l2_only
}

make_dummy_l2_counts <- function(items, T, Q, Gl2, level2_modifiers, arg) {
  array(0, c(T, Q, Gl2), list(arg$use_t, items, level2_modifiers))
}

factorize_arg_vars <- function(tabular, .arg) {
  arg_vars <- intersect(names(tabular),
    c(.arg$groups, .arg$geo_id, .arg$survey_id, .arg$level2_modifiers, .arg$level2_period1_modifiers))
  numeric_groups <- lapply(tabular, is.numeric)[arg_vars] %>% unlist()
  if (any(numeric_groups)) {
    message("Defining groups via numeric variables is allowed, but output names won't be descriptive. Consider using factors.")
    for (varname in arg_vars[numeric_groups]) {
      tabular[[varname]] <- paste0(varname, as.character(tabular[[varname]]))
    }
  }
  tabular <- tabular %>% dplyr::arrange_(.dots = arg_vars)
  tabular[arg_vars] <- with_contr.treatment(
    lapply(tabular[arg_vars], function(x) factor(x, levels = sort(unique(as.character(x))))))
  tabular
}

with_contr.treatment <- function(...) {
  contrast_options = getOption("contrasts")
  options("contrasts"= c(unordered = "contr.treatment", ordered = "contr.treatment"))
  res <- eval(...)
  options("contrasts"= contrast_options)
  res
}

drop_missing_respondents <- function(.data, .arg) {
  .data <- .data %>% dplyr::filter_(lazyeval::interp(~!none_valid))
  message(sprintf(ngettext(sum(.data$none_valid),
        "%i row has no responses",
        "%i rows have no responses"),
      sum(.data$none_valid)))
  .data
}

drop_missing_items <- function(.data, q_all_missing, .arg) {
  q_all_missing <- intersect(q_all_missing, names(.data))
  if (length(q_all_missing) > 0) {
    .data <- .data %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = q_all_missing))
  }
  message(sprintf(ngettext(length(q_all_missing),
        "%i question, %s, has no responses",
        "%i questions have no responses: %s"),
      length(q_all_missing),
      stringi::stri_c(q_all_missing, collapse = ", ")))
  .data
}

drop_rare_items_over_t <- function(.data, q_rare, .arg) {
  q_rare <- intersect(q_rare, names(.data))
  if (length(q_rare) > 0) {
    .data <- .data %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = q_rare))
  }
  message(sprintf(ngettext(length(q_rare),
        "%i question fails min_periods requirement (%i): %s",
        "%i questions fail min_periods requirement (%i): %s"),
      length(q_rare), .arg$min_periods,
      stringi::stri_c(q_rare, collapse = ", ")))
  .data
}

drop_rare_items_over_polls <- function(.data, .lt_min_surveys, .arg) {
  .lt_min_surveys <- intersect(.lt_min_surveys, names(.data))
  if (length(.lt_min_surveys) > 0) {
    .data <- .data %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = .lt_min_surveys))
  }
  message(sprintf(ngettext(length(.lt_min_surveys),
        "%i question fails min_surveys requirement (%i): %s",
        "%i questions fail min_surveys requirement (%i): %s"),
      length(.lt_min_surveys), .arg$min_surveys,
      stringi::stri_c(.lt_min_surveys, collapse = ", ")))
  .data
}

update_use_geo <- function(.data, .arg) {
  levels(unlist(.data[[.arg$geo_id]]))
}

set_use_t <- function(.data, .arg) {
  if (!length(.arg$periods) > 0) {
    return(sort(unique(.data[[.arg$time_id]])))
  } else {
    assertthat::assert_that(is.numeric(.arg$periods))
    return(sort(unique(.arg$periods)))
  }
}

subset_geos <- function(tabular, arg) {
  assertthat::assert_that(not_empty(tabular), is.data.frame(tabular), is_string(arg$geo_id))
  if (length(arg$geo_ids) < 1) {
    arg$geo_ids <- tabular[[arg$geo_id]]
  }
  assertthat::assert_that(not_empty(arg$geo_ids))
  geo_filter <- tabular[[arg$geo_id]] %in% arg$geo_ids
  tabular <- tabular %>% dplyr::filter(geo_filter)
  assertthat::assert_that(not_empty(tabular))
  tabular <- droplevels(tabular)
  return(tabular)
}


subset_to_estimation_periods <- function(.data, arg) {
  assertthat::assert_that(is.data.frame(.data), not_empty(.data)) 
  assertthat::assert_that(is_string(arg$time_id), is.numeric(.data[[arg$time_id]]), is.numeric(arg$use_t))
  .data <- .data %>% dplyr::filter_(lazyeval::interp(~period %in% arg$use_t, period = as.name(arg$time_id)))
  if (!nrow(.data) > 0) stop("no item data after restriction to estimation periods")
  .data <- droplevels(.data)
  return(.data)
}

drop_rows_missing_covariates <- function(.data, covariates, .arg) {
  n <- nrow(.data)
  .data <- .data %>%
    dplyr::filter_(lazyeval::interp(~!is.na(geo_name) & !is.na(time_name),
  geo_name = as.name(.arg$geo_id),
  time_name = as.name(.arg$time_id)))
  for (v in covariates) {
    .data <- .data %>%
      dplyr::filter_(lazyeval::interp(~!is.na(varname), varname = as.name(v)))
  }
  message(n - nrow(.data), " rows dropped for missingness in covariates")
  droplevels(.data)
}

drop_rows_missing_items <- function(.data, .arg) {
  item_filter <- rowSums(!is.na(.data[, .arg$items, drop = FALSE])) > 0
  .data <- .data %>% dplyr::filter(item_filter)
  # droplevels(.data)
}

subset_to_observed_geo_periods <- function(level2, arg) {
  # Subset level-2 data to geographic units observed and time periods
  # specified
  if (length(arg$level2) < 1) {
    return(NULL)
  } else {
    level2 <- level2 %>%
      dplyr::filter_(lazyeval::interp(~geo %in% arg$use_geo, geo = as.name(arg$geo_id)))
    assertthat::assert_that(not_empty(level2))
  }
  return(level2)
}

get_gt <- function(level1) {
  gts <- level1 %>% dplyr::select_(lazyeval::interp(~matches(x), x = "_gt\\d+$"))
  assertthat::assert_that(not_empty(gts))
  return(gts)
}

count_respondent_trials <- function(level1) {
  gts <- get_gt(level1)
  rowSums(!is.na(as.matrix(gts)), na.rm = TRUE)
}

count_questions <- function(level1) {
  sum(grepl("_gt", colnames(level1), fixed = TRUE))
}

count_covariate_combos <- function(tab, arg) {
  factor_levels <- nlevels_vectorized(tab, c(arg$geo_id, arg$groups))
  Reduce(`*`, factor_levels)
}

nlevels_vectorized <- function(data, varlist) {
  sapply(data[varlist], nlevels)
}

handle_data <- function(.data, covariates, factorize, .arg) {
  .data <- as_tbl(.data)
  # Make all the variables given as strings factors
  # Drop rows lacking covariates, time variable, or geographic variable
  .data <- drop_rows_missing_covariates(.data, covariates, .arg)
  if (factorize == TRUE) {
    .data <- factorize_arg_vars(.data, .arg)
  }
  return(.data)
}

apply_restrictions <- function(.data, .checks, .arg) {
  # Apply geo filter
  .data <- subset_geos(.data, .arg)
  # Drop rows with no valid question responses
  .data <- drop_missing_respondents(.data, .arg)
  # Drop variables columns with no valid responses
  .data <- drop_missing_items(.data, .checks$q_all_missing, .arg)
  # Drop variables that don"t satisfy min_periods requirement
  .data <- drop_rare_items_over_t(.data, .checks$q_rare.t, .arg)
  assertthat::assert_that(not_empty(.data), not_empty(.arg$items))
  .data <- drop_rare_items_over_polls(.data, .checks$q_rare_polls, .arg)
  .data
}

count_level2_groups <- function(level2, G, arg) {
  if (length(level2) < 1) {
    l2.group <- gl(1, G)
    Gl2 <- nlevels(l2.group)
    return(Gl2)
  } else {
    Gl2 <- length(arg$level2_modifiers)
    Gl2 <- max(unlist(Gl2), 1)
    return(Gl2)
  }
}

make_group_grid <- function(level, factors, arg) {
  assertthat::assert_that(is.numeric(arg$use_t), not_empty((arg$use_t)), is_string(arg$geo_id), is.character(factors))
  assertthat::assert_that(is_string(arg$time_id), is.numeric(level[[arg$time_id]]))
  group_grid <- expand.grid(c(
    setNames(list(arg$use_t), arg$time_id),
    lapply(level[c(arg$geo_id, factors)], function(x) sort(unique(x)))))
  assertthat::assert_that(not_empty(group_grid))
  group_grid
}

muffle_full_join <- function(...) {
  suppressWarnings(dplyr::full_join(...))
}

# Create summary table of design effects
create_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}

# Create design matrix for model of hierarchical coefficients
create_l2_design_matrix <- function(group_design_matrix, arg) {
  if (is.null(arg$level2_modifiers)) {
    zz.names <- list(arg$use_t, dimnames(group_design_matrix)[[2]], "Zero")
    ZZ <- array(data = 0, dim = lapply(zz.names, length),
      dimnames = zz.names)
  } else {

  }
  assertthat::assert_that(none_in(ZZ, NA))
  return(ZZ)
}

# Create 'greater than' indicators
create_gt_variables <- function(d, .items){
  out <- lapply(.items, function(item) {
    if (is.ordered(d[[item]])) {
      item_levels <- na.omit(levels(droplevels(d[[item]])))
      values <- match(as.character(d[[item]]), item_levels)
    } else if (is.numeric(d[[item]])) {
      item_levels <- sort(na.omit(unique(d[[item]])))
      values <- match(d[[item]], item_levels)
    } else {
      stop("each item should be an ordered factor or numeric")
    }
    message("'", item, "' is class '", class(d[[item]]), "' with ", length(item_levels),
      " non-missing values: '", paste(item_levels, collapse = "', '"), "'")
    gt_levels <- seq_along(item_levels)[-length(item_levels)]
    if (length(gt_levels) < 1) stop("no variation in item ", deparse(item))
    if (identical(length(gt_levels), 1L)) {
      assertthat::assert_that(has_length(item_levels, 2))
      message("\t considered binary with failure='", item_levels[1], "' and success='", item_levels[2], "'")
    }
    if (length(gt_levels) > 1L) {
      assertthat::assert_that(length(item_levels) > 2L)
      message("\t considered ordinal with levels '", paste(item_levels, collapse = "', '"), "' (ascending)")
    }
    gt_cols <- lapply(gt_levels, function(gt) {
      ifelse(values > gt, 1L, 0L)
    })
    assertthat::assert_that(not_empty(gt_cols))
    gt_names <- paste(item, gt_levels, sep = "_gt")
    setNames(gt_cols, gt_names)
  })
  dplyr::bind_cols(out)
}
