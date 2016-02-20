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

  # data = list(level1 = state_opinion)
  # vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
  #             groups = c("race"),
  #             time_id = "year",
  #             geo_id = "state",
  #             survey_id = "source",
  #             survey_weight = "weight")
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

  arg <- handle_arguments()
  item <- itemize(arg)

  arg$periods <- set_use_t(item, arg)
  arg$use_geo <- update_use_geo(item, arg)
  item$filters <- add_filters(arg)

  item <- handle_data(item,
    covariates = c(item$time, item$geo, item$groups, item$survey),
    factorize = TRUE,
    arg = arg)
  if (length(arg$level2) > 0) {
    arg$level2 <- handle_data(arg$level2,
      covariates = unique(c(item$time, item$geo, arg$level2_modifiers,
          arg$level2_period1_modifiers)),
      # TODO: handle
      factorize = FALSE,
      arg = arg)
  }

  ## INDIVIDUAL LEVEL ##
  item$restrict()

  # Create weights from population targets
  if (length(arg$targets) > 0) {
    item <- create_weights(item, arg)
  }

  item <- add_gt_variables(item, arg)
  item <- drop_rows_missing_items(item, arg)

  # Fix factor levels after filtering
  item$tbl <- droplevels(item$tbl)
  arg$level2 <- subset_to_observed_geo_periods(arg$level2, arg)
  nonmissing_t <- sort(unique(item$tbl[[item$time]]))

  group_grid <- make_group_grid(item, item$groups, arg) %>%
    dplyr::arrange_(.dots = c(item$time, item$groups, item$geo))
  group_grid_t <- group_grid %>%
    dplyr::select_(lazyeval::interp(~-one_of(v), v = item$time)) %>%
    dplyr::distinct() %>%
    dplyr::arrange_(.dots = c(item$groups, item$geo))

  group_levels <- vapply(item$groups, function(i) length(levels(group_grid_t[[i]])), numeric(1))
  if (any(group_levels < 2)) {
    stop("no variation in group variable: ", paste(names(which(group_levels < 2)), collapse = ", "))
  }

  group_counts <- make_group_counts(item, group_grid, arg)
  MMM <- create_missingness_array(group_counts, group_grid, arg)

  T <- length(item$filters$t)
  Q <- count_questions(item)
  G <- count_covariate_combos(item, arg)

  # Create placeholders
  # TODO: confirm that count_level2_groups works with > 1 group
  Gl2 <- count_level2_groups(arg$level2, G, arg)
  WT <- make_dummy_weight_matrix(T, Gl2, G)
  l2_only <- make_dummy_l2_only(item, arg)
  NNl2 <- make_dummy_l2_counts(item, T, Q, Gl2 = Gl2, arg$level2_modifiers, arg)
  SSl2 <- make_dummy_l2_counts(item, T, Q, Gl2 = Gl2, arg$level2_modifiers, arg)

  group_design_matrix <- make_design_matrix(group_grid_t, item$groups, arg)
  ZZ <- make_hierarchical_array(item, arg$level2, arg$level2_modifiers, group_design_matrix, group_grid_t, arg) 
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
    vars = list(items = item$items,
                gt_items = grep("_gt\\d+$", colnames(item$tbl), value = TRUE),
                groups = item$groups,
                time_id = item$time,
                use_t = item$filters$t,
                geo_id = item$geo,
                periods = arg$periods,
                survey_id = item$survey,
                covariate_groups = group_grid_t,
                hier_names = dimnames(group_design_matrix)[[2]]))

  # Check dimensions against what Stan will expect
  check_dimensions(stan_data)
  check_values(stan_data)
  check_order(stan_data)

  return(stan_data)
}

make_hierarchical_array <- function(item, level2, level2_modifiers, group_design_matrix, group_grid_t, arg) {
  if (length(level2) > 0) {
    ZZ <- shape_hierarchical_data(level2, level2_modifiers, group_grid_t, arg)
  } else {
    zz.names <- list(item$filters$t, dimnames(group_design_matrix)[[2]], "")
    ZZ <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  }
  ZZ
}

shape_hierarchical_data <- function(level2, modifiers, group_grid_t, arg) {
  # the array of hierarchical data ZZ should be T x P x H, where T is the number of time periods, P is the number of
  # hierarchical parameters (including the geographic), and H is the number of predictors for geographic unit effects
  # TODO: make flexible; as written we must model geo x t
  modeled_params = c(item$geo, item$time)
  unmodeled_params = setdiff(c(item$geo, item$time, item$groups), modeled_params)
  # TODO: uniqueness checks on level2 data (state_demographics should not pass)
  # TODO: confirm sort order of level2_modifiers
  missing_level2_geo <- setdiff(
    unique(unlist(dplyr::select_(group_grid_t, item$geo))),
    unique(unlist(dplyr::select_(level2, item$geo))))
  if (length(missing_level2_geo) > 0) stop("no hierarchical data for geo in item data: ", missing_level2_geo)
  missing_level2_t <- setdiff(item$filters$t, unlist(dplyr::select_(level2, item$time)))
  if (length(missing_level2_t) > 0) stop("missing hierarchical data for t in item data: ", missing_level2_t)

  hier_frame <- level2 %>% dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~paste0(item$geo, geo),
      geo = as.name(item$geo))), item$geo)) %>%
    dplyr::select_(.dots = c(modeled_params, arg$level2_modifiers)) %>%
    dplyr::rename_("param" = item$geo) %>%
    dplyr::mutate_("param" = ~as.character(param)) %>%
    dplyr::arrange_(.dots = c("param", item$time))

  modeled_param_names <- unique(unlist(dplyr::select_(hier_frame, "param")))
  unmodeled_param_levels = lapply(unmodeled_params, function(x) {
      paste0(x, levels(group_grid_t[[x]]))[-1]
    }) %>% unlist()
  param_levels <- c(modeled_param_names, unmodeled_param_levels)

  unmodeled_frame <- expand.grid(c(list(
      unmodeled_param_levels, 
      sort(unique(unlist(dplyr::select_(level2, item$time))))),
      rep(list(0), length(modifiers)))) %>%
    setNames(c("param", item$time, modifiers)) %>%
    dplyr::arrange_(.dots = c("param", item$time))

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
  hier_melt <- wrap_melt(hier_frame, id.vars = c("param", item$time), variable.name = "modifiers") %>%
    dplyr::mutate_("param" = ~factor(param, levels = param_levels, ordered = TRUE))
  if (!inherits(hier_melt$value, "numeric")) stop("non-numeric values in hierarchical data. Factor handling probably failed. Possible quickfix: omit or manually dummy out any factors in 'level2_modifiers' or 'level2_period1_modifiers'.")
  assertthat::assert_that(names_subset(modeled_param_names, unlist(hier_melt$param)))
  assertthat::assert_that(names_subset(unmodeled_param_levels, unlist(hier_melt$param)))
  melt_formula <- as.formula(paste(item$time, "param", "modifiers", sep = " ~ "))
  zz <- reshape2::acast(hier_melt, melt_formula, drop = FALSE, value.var = "value")
  zz <- zz[, -1, , drop = FALSE]
  zz
}

add_gt_variables <- function(item, arg) {
  gt_table <- create_gt_variables(item)
  item$tbl <- dplyr::bind_cols(item$tbl, gt_table)
  item
}


make_group_counts <- function(item, group_grid, arg) {
  item$tbl$n_responses <- count_respondent_trials(item)
  group_design_effects <- compute_group_design_effects(item, arg)
  group_trial_counts <- count_group_trials(item, group_design_effects,
    group_grid, arg)
  mean_group_outcome <- compute_mean_group_outcome(item, group_grid, arg)
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

check_restrictions <- function(item, arg) {
  checks <- list()
  # # We need to find out which time periods are observed in the data
  # checks$observed_periods <- get_observed_t(item$tbl[[item$time]])
  #
  # Which item columns have no valid responses?
  checks$q_all_missing <- get_missing_items(item, item$items)

  # Which variables don't satisfy the min_periods requirement?
  checks$q_when_asked <- get_question_periods(item, arg)
  checks$q_rare.t <- get_rare_items_over_t(checks, arg)

  # Which variables don't satisfy the min_surveys requirement?
  checks$q_which_asked <- get_question_polls(item, arg)
  checks$q_rare_polls <- get_rare_items_over_polls(checks, arg)

  return(checks)
}

get_question_periods <- function(item, arg) {
  question_periods <- item$tbl %>%
    dplyr::group_by_(item$time) %>%
    dplyr::summarise_each_(~anyValid, vars = item$items)
  question_periods
}

# checks <- list(q_when_asked = get_question_periods(d, a))
# arg = list(time_id = "t", min_periods = 1L)
# get_rare_items_over_t(checks, arg)

get_rare_items_over_t <- function(.checks, arg) {
  assertthat::assert_that(is_count(arg$min_periods))
  q_t_count <- colSums(dplyr::select_(.checks$q_when_asked,
    lazyeval::interp(~-one_of(v), v = item$time)))
  q_rare <- names(q_t_count)[q_t_count < arg$min_periods]
  return(q_rare)
}

get_question_polls <- function(item, arg) {
  question_polls <- item$tbl %>%
    dplyr::group_by_(item$survey) %>%
    dplyr::summarise_each_(dplyr::funs(anyValid), vars = item$items)
  question_polls
}

get_rare_items_over_polls <- function(.checks, arg) {
  q_counts <- .checks$q_which_asked %>%
    dplyr::select_(lazyeval::interp(~-one_of(v),
      v = item$survey)) %>%
    dplyr::summarise_each(~sum)
  lt_min_surveys <- colnames(q_counts)[unlist(q_counts) < arg$min_surveys]
  lt_min_surveys
}

get_observed_t <- function(t_data) {
  observed_t <- unlist(t_data)
  assertthat::assert_that(is.numeric(observed_t), not_empty(observed_t))
  unique(observed_t)
}

get_missing_items <- function(item, item_names) {
  nonmissing <- item$tbl %>%
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

compute_group_design_effects <- function(item, arg) {
  assertthat::assert_that(is_numeric(item$tbl[[item$weight]]))
  assertthat::assert_that(all_strings(c(item$weight, item$geo, item$time)), all_strings(item$groups))
  assertthat::assert_that(has_all_names(item$tbl, c(item$weight, item$geo, item$time)),
    has_all_names(item$tbl, item$groups))

  de_table <- dplyr::as.tbl(item$tbl) %>%
    dplyr::group_by_(.dots = c(item$geo, item$groups, item$time)) %>%
    dplyr::select_(item$weight) %>%
    dplyr::summarise_("def" = lazyeval::interp(~create_design_effects(w),
  w = as.name(item$weight))) %>%
    dplyr::arrange_(.dots = c(item$geo, item$groups, item$time))

  assertthat::assert_that(has_all_names(de_table, c("def", item$geo, item$groups, item$time)))
  return(de_table)
}

count_group_trials <- function(item, design_effects, group_grid, arg) {
  assertthat::assert_that(not_empty(item$tbl), length(grep("_gt\\d+$", colnames(item$tbl))) > 0)
  not_na_trial <- item$tbl %>%
    # The _gt variables can take values of 0/1/NA
    dplyr::mutate_each_(~notNA, ~matches("_gt\\d+$"))
    # For a respondent who only answered questions A and B, we now have
    #  T,   T,   T,   T,   T,   F,   F.
  assertthat::assert_that(not_empty(not_na_trial))
  trial_counts <- not_na_trial %>%
    dplyr::mutate_each_(~. / n_responses, vars = ~matches("_gt\\d+$")) %>%
    # Dividing the booleans by n_responses = 5 calculated earlier gives us
    #.02, .02, .02, .02, .02,   0,   0.
    dplyr::group_by_(.dots = c(item$geo, item$groups, item$time)) %>%
    dplyr::summarise_each_(~sum, vars = ~matches("_gt\\d+$")) %>%
    dplyr::ungroup() %>%
    # Summing over respondents within the grouping variables gives the count
    # of trials for each combination of geo, demo, and time variables, divided
    # equally across the _gt indicators.
    # Joining the design effect table by the grouping variables attaches
    # the design effect associated with each variable combination.
    muffle_full_join(design_effects, by = c(item$geo, item$groups, item$time)) %>%
    dplyr::mutate_each_(~ceiling(. / def), vars = ~matches("_gt\\d+$")) %>%
    # Dividing by the design effect gives us a design-effect-adjusted trial
    # count.
    dplyr::select_(~-def)# %>%
    # Tidy up.
    # dplyr::mutate_each_(~as.character, vars = c(item$geo, item$groups, item$time))

  # A dplyr::full_join of trial_counts and group_grid will create rows for the desired
  # covariate combinations, where unobserved cells take NA values
  trial_counts <- muffle_full_join(trial_counts, group_grid,
    by = c(item$geo, item$groups, item$time))
  # Replace the NA values in those rows representing unobserved covariate
  # combinations with zeroes
  trial_counts <- trial_counts %>% dplyr::mutate_each_(~replaceNA, ~matches("_gt\\d+$"))
  # NOTE: Order will matter in a moment
  trial_counts <- trial_counts %>%
    dplyr::arrange_(.dots = c(item$geo, item$groups, item$time)) %>%
    dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  return(trial_counts)
}

compute_mean_group_outcome <- function(item, group_grid, arg) {
  mean_y <- item$tbl %>%
    dplyr::group_by_(.dots = c(item$geo, item$groups, item$time)) %>%
    # subset to identifiers and items
    dplyr::select_(~matches("_gt\\d+$"), ~n_responses, item$weight) %>%
    # weight by n_responses
    dplyr::mutate_("weight" = lazyeval::interp(~ w / n,
      w = as.name(item$weight), n = quote(n_responses))) %>%
    # take weighted mean of item responses within geo, group, time
    dplyr::summarise_each_(~weighted.mean(as.vector(.), weight,
      na.rm = TRUE), vars = "contains(\"_gt\")") %>%
    dplyr::group_by_(.dots = c(item$geo, item$groups, item$time)) %>%
    # replace NaN
    dplyr::mutate_each(~replaceNaN) %>%
    dplyr::ungroup()

  # make sure missing group appear as NA
  mean_y <- muffle_full_join(mean_y, group_grid,
    by = c(item$geo, item$groups, item$time)) %>%
      # NOTE: Order will matter in a moment
      dplyr::arrange_(.dots = c(item$geo, item$groups, item$time)) %>%
      dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  return(mean_y)
}

count_group_successes <- function(group_trial_counts, mean_group_outcome, arg) {
  # Confirm row order is identical before taking product
  assertthat::assert_that(all_equal(
      dplyr::select_(mean_group_outcome, .dots = c(item$geo, item$groups, item$time)),
      dplyr::select_(group_trial_counts, .dots = c(item$geo, item$groups, item$time))))
  success_counts <- get_gt(group_trial_counts) * get_gt(mean_group_outcome)
  success_counts <- success_counts %>%
    # Reattach our identifiers
    dplyr::bind_cols(dplyr::select_(group_trial_counts, .dots = c(item$geo, item$groups, item$time)), .) %>%
    # Round off returning integers and replace NA with 0
    dplyr::ungroup() %>%
    dplyr::mutate_each_(~replaceNA, ~matches("_gt\\d+$")) %>%
    dplyr::mutate_each_(~round(., digits = 0), ~matches("_gt\\d+$")) %>%
    dplyr::arrange_(.dots = c(item$geo, item$groups, item$time))
  return(success_counts)
}

make_design_matrix <- function(group_grid_t, factors, arg) {
  design_formula <- as.formula(paste("~ 0", item$geo, paste(factors, collapse = " + "), sep = " + "))
  design_matrix <- with_contr.treatment(model.matrix(design_formula, group_grid_t))
  design_matrix <- cbind(design_matrix, group_grid_t) %>%
    dplyr::arrange_(.dots = c(factors, item$geo))
  group_names <- concat_groups(group_grid_t, factors, item$geo, "name")
  design_matrix <- concat_groups(design_matrix, factors, item$geo, "name")
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

summarize_trials_by_period <- function(trial_counts, arg) {
  dplyr::select_(trial_counts, ~matches("_gt\\d+$"), item$time) %>%
    reshape2::melt(id.vars = item$time) %>%
    dplyr::group_by_(item$time) %>%
    dplyr::summarise_(valid_items = ~sum(value, na.rm = TRUE) > 0)
}

format_counts <- function(trial_counts, success_counts, arg) {
  trial_counts_melt <- wrap_melt(trial_counts, variable.name = "item",
    id.vars = c(item$geo, item$groups, item$time), value.name = "n_grp")
  success_counts_melt <- wrap_melt(success_counts, variable.name = "item",
    id.vars = c(item$geo, item$groups, item$time), value.name = "s_grp")
  joined <- dplyr::left_join(trial_counts_melt, success_counts_melt,
      by = c(item$geo, item$groups, item$time, "item")) %>%
    tidyr::unite_("name", c(item$geo, item$groups), sep = "__", remove = FALSE) %>%
    dplyr::filter_(~n_grp != 0) %>%
    dplyr::mutate_(
      n_grp = ~replaceNA(n_grp),
      s_grp = ~replaceNA(s_grp),
      name = lazyeval::interp(~paste(period, item, name, sep = " | "), period = as.name(item$time))) %>%
      # NOTE: arrange by time, item, group. Otherwise dgirt() output won't have
      # the expected order.
    dplyr::arrange_(.dots = c(item$time, "item", item$groups, item$geo))
  joined
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
  melt
}

get_missingness <- function(group_counts, group_grid, arg) {
  group_counts %>%
    # Include in the missingness array all group-variables that might not exist
    # in the data because of unobserved use_t
    muffle_full_join(group_grid, by = c(item$geo, item$groups, item$time)) %>%
    # Get missingness by group
    dplyr::group_by_(.dots = c("item", item$time, item$groups, item$geo)) %>%
    dplyr::summarise_("m_grp" = ~as.integer(sum(n_grp, na.rm = TRUE) == 0)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(.dots = c(item$time, "item", item$groups, item$geo))
}

cast_missingness <- function(missingness, arg) {
  acast_formula <- as.formula(paste0(item$time, "~ item ~", paste(item$groups, collapse = "+"), "+", item$geo))
  MMM <- missingness %>%
    dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~paste0("x_", geo), geo = as.name(item$geo))), item$geo)) %>%
    dplyr::arrange_(.dots = c(item$time, "item", item$groups, item$geo)) %>%
    reshape2::acast(acast_formula, value.var = "m_grp", fill = 1)
  # But we don"t want to include the character string "NA" as a variable
  MMM <- MMM[
    !(dimnames(MMM)[[1]] == "NA"),
    !(dimnames(MMM)[[2]] == "NA"),
    !(dimnames(MMM)[[3]] == "NA"), drop = FALSE]
  # No cells should be NA either
  assertthat::assert_that(all_in(MMM, c(0, 1)))
  MMM
}

make_dummy_weight_matrix <- function(T, Gl2, G) {
  array(1, dim = c(T, Gl2, G))
}

make_dummy_l2_only <- function(item, arg) {
  gt_names <- grep("_gt\\d+$", colnames(item$tbl), value = TRUE)
  l2_only <- matrix(0L,
    nrow = length(item$filters$t),
    ncol = length(gt_names),
    dimnames = list(item$filters$t, gt_names))
  l2_only
}

make_dummy_l2_counts <- function(item, T, Q, Gl2, level2_modifiers, arg) {
  array(0, c(T, Q, Gl2), list(item$filters$t,
      grep("_gt", colnames(item$tbl), fixed= TRUE, value = TRUE), level2_modifiers))
}

factorize_arg_vars <- function(item, arg) {
  arg_vars <- intersect(names(item$tbl),
    c(item$groups, item$geo, item$survey, arg$level2_modifiers, arg$level2_period1_modifiers))
  numeric_groups <- item$tbl %>% dplyr::summarize_each_(~is.numeric, vars = arg_vars) %>% unlist()
  if (any(numeric_groups)) {
    message("Defining groups via numeric variables is allowed, but output names won't be descriptive. Consider using factors.")
    for (varname in names(numeric_groups)) {
      item$tbl[[varname]] <- paste0(varname, as.character(item$tbl[[varname]]))
    }
  }
  item$tbl <- with_contr.treatment(item$tbl %>% 
    dplyr::arrange_(.dots = arg_vars) %>%
    dplyr::mutate_each_(dplyr::funs(factor(., levels = sort(unique(as.character(.))))), vars = arg_vars))
  item
}

with_contr.treatment <- function(...) {
  contrast_options = getOption("contrasts")
  options("contrasts"= c(unordered = "contr.treatment", ordered = "contr.treatment"))
  res <- eval(...)
  options("contrasts"= contrast_options)
  res
}

drop_missing_respondents <- function(item, arg) {
  item$tbl <- item$tbl %>% dplyr::filter_(lazyeval::interp(~!none_valid))
  message(sprintf(ngettext(sum(item$tbl$none_valid),
        "%i row has no responses",
        "%i rows have no responses"),
      sum(item$tbl$none_valid)))
  item
}

drop_missing_items <- function(item, q_all_missing, arg) {
  q_all_missing <- intersect(q_all_missing, names(item$tbl))
  if (length(q_all_missing) > 0) {
    item$tbl <- item$tbl %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = q_all_missing))
  }
  message(sprintf(ngettext(length(q_all_missing),
        "%i question, %s, has no responses",
        "%i questions have no responses: %s"),
      length(q_all_missing),
      stringi::stri_c(q_all_missing, collapse = ", ")))
  item
}

drop_rare_items_over_t <- function(item, q_rare, arg) {
  q_rare <- intersect(q_rare, names(item$tbl))
  if (length(q_rare) > 0) {
    item$tbl <- item$tbl %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = q_rare))
  }
  message(sprintf(ngettext(length(q_rare),
        "%i question fails min_periods requirement (%i): %s",
        "%i questions fail min_periods requirement (%i): %s"),
      length(q_rare), arg$min_periods,
      stringi::stri_c(q_rare, collapse = ", ")))
  item
}

drop_rare_items_over_polls <- function(item, .lt_min_surveys, arg) {
  .lt_min_surveys <- intersect(.lt_min_surveys, names(item$tbl))
  if (length(.lt_min_surveys) > 0) {
    item$tbl <- item$tbl %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = .lt_min_surveys))
  }
  message(sprintf(ngettext(length(.lt_min_surveys),
        "%i question fails min_surveys requirement (%i): %s",
        "%i questions fail min_surveys requirement (%i): %s"),
      length(.lt_min_surveys), arg$min_surveys,
      stringi::stri_c(.lt_min_surveys, collapse = ", ")))
  item
}

update_use_geo <- function(item, arg) {
  levels(unlist(item$tbl[[item$geo]]))
}

set_use_t <- function(item, arg) {
  if (!length(arg$periods) > 0) {
    return(unique(item$tbl[[item$time]]))
  } else {
    assertthat::assert_that(is.numeric(arg$periods))
    return(arg$periods)
  }
}

subset_geos <- function(item, arg) {
  # TODO: add filter 
  # geo_filter <- item$tbl[[item$geo]] %in% FILTER$geo 
  item$tbl <- item$tbl %>% dplyr::filter(geo_filter)
  assertthat::assert_that(not_empty(item$tbl))
  item$tbl <- droplevels(item$tbl)
  item
}


subset_to_estimation_periods <- function(item, arg) {
  assertthat::assert_that(is.data.frame(item$tbl), not_empty(item$tbl)) 
  assertthat::assert_that(is_string(item$time), is.numeric(item$tbl[[item$time]]), is.numeric(item$filters$t))
  periods_filter <- item$tbl[[item$time]] %in% item$filters$t
  item$tbl <- item$tbl %>% dplyr::filter(periods_filter)
  if (!nrow(item$tbl) > 0) stop("no item data after restriction to estimation periods")
  item$tbl <- droplevels(item$tbl)
  item
}

drop_rows_missing_covariates <- function(item, covariates, arg) {
  n <- nrow(item$tbl)
  item$tbl <- item$tbl %>%
    dplyr::filter_(lazyeval::interp(~!is.na(geo_name) & !is.na(time_name),
  geo_name = as.name(item$geo),
  time_name = as.name(item$time)))
  for (v in covariates) {
    item$tbl <- item$tbl %>%
      dplyr::filter_(lazyeval::interp(~!is.na(varname), varname = as.name(v)))
  }
  message(n - nrow(item$tbl), " rows dropped for missingness in covariates")
  item$tbl <- droplevels(item$tbl)
  item
}

drop_rows_missing_items <- function(item, arg) {
  item_filter <- rowSums(!is.na(item$tbl[, item$items, drop = FALSE])) > 0
  item$tbl <- item$tbl %>% dplyr::filter(item_filter)
  item$tbl <- droplevels(item$tbl)
  item
}

subset_to_observed_geo_periods <- function(level2, arg) {
  # Subset level-2 data to geographic units observed and time periods
  # specified
  if (length(arg$level2) < 1) {
    return(NULL)
  } else {
    level2 <- level2 %>%
      dplyr::filter_(lazyeval::interp(~geo %in% item$filter$geo, geo = as.name(item$geo)))
    assertthat::assert_that(not_empty(level2))
  }
  return(level2)
}

get_gt <- function(tbl) {
  gts <- tbl %>% dplyr::select_(lazyeval::interp(~matches(x), x = "_gt\\d+$"))
  return(gts)
}

count_respondent_trials <- function(item) {
  gts <- get_gt(item$tbl)
  rowSums(!is.na(as.matrix(gts)), na.rm = TRUE)
}

count_questions <- function(item) {
  sum(grepl("_gt", colnames(item$tbl), fixed = TRUE))
}

count_covariate_combos <- function(item, arg) {
  factor_levels <- nlevels_vectorized(item$tbl, c(item$geo, item$groups))
  Reduce(`*`, factor_levels)
}

nlevels_vectorized <- function(data, varlist) {
  sapply(data[varlist], nlevels)
}

handle_data <- function(item, covariates, factorize, arg) {
  item$tbl <- as_tbl(item$tbl)
  # Make all the variables given as strings factors
  # Drop rows lacking covariates, time variable, or geographic variable
  item <- drop_rows_missing_covariates(item, covariates, arg)
  if (factorize == TRUE) {
    item <- factorize_arg_vars(item, arg)
  }
  item
}

apply_restrictions <- function(item, .checks, arg) {
  # Apply geo filter
  # item$tbl <- subset_geos(item$tbl = item$tbl, arg)
  # Drop rows with no valid question responses
  item$tbl <- drop_missing_respondents(item$tbl, arg)
  # Drop variables columns with no valid responses
  item$tbl <- drop_missing_items(item$tbl, .checks$q_all_missing, arg)
  # Drop variables that don"t satisfy min_periods requirement
  item$tbl <- drop_rare_items_over_t(item$tbl, .checks$q_rare.t, arg)
  assertthat::assert_that(not_empty(item$tbl), not_empty(item$items))
  item$tbl <- drop_rare_items_over_polls(item$tbl, .checks$q_rare_polls, arg)
  item
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

make_group_grid <- function(item, factors, arg) {
  assertthat::assert_that(is.numeric(item$filters$t), not_empty((item$filters$t)), is_string(item$geo), is.character(factors))
  assertthat::assert_that(is_string(item$time), is.numeric(item$tbl[[item$time]]))
  group_grid <- expand.grid(c(
    setNames(list(item$filters$t), item$time),
    lapply(item$tbl[, c(item$geo, factors), drop = FALSE], function(x) sort(unique(x)))))
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
    zz.names <- list(item$filters$t, dimnames(group_design_matrix)[[2]], "Zero")
    ZZ <- array(data = 0, dim = lapply(zz.names, length),
      dimnames = zz.names)
  } else {

  }
  assertthat::assert_that(none_in(ZZ, NA))
  return(ZZ)
}

# Create 'greater than' indicators
create_gt_variables <- function(item){
  widths <- c("item" = 30, "class" = 10, "levels" = 12, "responses" = 16)
  print_varinfo_header(widths)
  out <- lapply(item$items, function(i) {
    if (is.ordered(item$tbl[[i]])) {
      i_levels <- na.omit(levels(droplevels(item$tbl[[i]])))
      values <- match(as.character(item$tbl[[i]]), i_levels)
    } else if (is.numeric(item$tbl[[i]])) {
      i_levels <- sort(na.omit(unique(item$tbl[[i]])))
      values <- match(item$tbl[[i]], i_levels)
    } else {
      stop("each item should be an ordered factor or numeric")
    }
    gt_levels <- seq_along(i_levels)[-length(i_levels)]
    if (length(gt_levels) < 1) stop("no variation in item ", deparse(i))
    if (identical(length(gt_levels), 1L)) {
      assertthat::assert_that(has_length(i_levels, 2))
    }
    if (length(gt_levels) > 1L) {
      assertthat::assert_that(length(i_levels) > 2L)
    }
    print_varinfo(item$tbl, i, item_levels = i_levels, gt_levels = gt_levels, widths)
    gt_cols <- lapply(gt_levels, function(gt) {
      ifelse(values > gt, 1L, 0L)
    })
    assertthat::assert_that(not_empty(gt_cols))
    gt_names <- paste(i, gt_levels, sep = "_gt")
    setNames(gt_cols, gt_names)
  })
  print_varinfo_rule(widths)
  dplyr::bind_cols(out)
}

print_varinfo_header <- function(widths) {
  pad_side <- c("right", rep("left", length(widths) - 1))
  message(concat_varinfo(widths, names(widths), pad_side))
  print_varinfo_rule(widths)
}

print_varinfo_rule <- function(widths) {
  message(paste0(rep('-', sum(widths)), collapse = ''))
}

print_varinfo <- function(d, item, item_levels, gt_levels, widths) {
  item_name = ifelse(nchar(item) > 30, paste0(stringi::stri_sub(item, 1, 26), '...'), item)
  responses = format(sum(!is.na(d[[item]])), big.mark = ",")
  values <- c("item" = item_name, "class" = class(d[[item]]), "levels" = length(item_levels), responses = responses)
  pad_side <- c("right", rep("left", length(values) - 1))
  assertthat::assert_that(identical(length(widths), length(values)))
  message(concat_varinfo(widths, values, pad_side))
}

concat_varinfo = function(widths, values, pad_side) {
  mapply(function(width, value, side) {
      paste0(stringi::stri_pad(value, width, side = side), collapse = '')
    }, widths, values, pad_side)
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
