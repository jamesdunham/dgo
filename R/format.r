#'Format data for use with dgirt
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
#'     \item{periods}{A character vector of time periods to be used in estimation,
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
#'     \item{difficulty_count}{Number of difficulty parameters.}
#'     \item{delta_tbar_prior_mean}{Prior mean of $\bar{\delta_t}$.}
#'     \item{delta_tbar_prior_sd}{Prior standard deviation of $\bar{\delta_t}$.}
#'     \item{innov_sd_delta_scale}{Prior scale of innovation parameter for standard deviation of $bar{\delta_t}$.}
#'     \item{innov_sd_theta_scale{Prior scale of innovation for standard deviation of group ability parameter $bar{\theta_t}$.}
#'   }
#' @return \code{list} List formatted for `run_dgirt`.
#' @export

format_data <- function(data = list(level1,
                                    level2 = NULL,
                                    targets = NULL),
                       vars = list(items,
                                   groups,
                                   time_id,
                                   geo_id,
                                   survey_id,
                                   survey_weight,
                                   preweights = NULL,
                                   preweight_proportion = NULL,
                                   level2_modifiers = NULL,
                                   level2_period1_modifiers = NULL),
                       filters = list(periods = NULL,
                                      geo_ids = NULL,
                                      min_surveys = 1L,
                                      min_periods = 1L),
                       params = list(separate_periods = FALSE,
                                     difficulty_count = 1L,
                                     constant_item = TRUE,
                                     delta_tbar_prior_mean = 0.5,
                                     delta_tbar_prior_sd = 0.5,
                                     innov_sd_delta_scale = 2.5,
                                     innov_sd_theta_scale = 2.5)) {

  arg <- handle_arguments()
  level1 <- handle_data(arg$level1, arg)
  arg$level2 <- handle_data(arg$level2, arg)

  ## INDIVIDUAL LEVEL ##

  arg$demo_id <- arg$groups

  # Drop rows lacking covariates, time variable, or geographic variable
  level1 <- drop_rows_missing_covariates(level1, arg)

  # Filter data
  checks <- check_restrictions(level1, arg)
  # Find rows with no valid question responses
  level1$none_valid <- get_missing_respondents(level1[, arg$items])
  level1 <- apply_restrictions(level1, checks, arg)
  # Update arg$items
  arg$items <- intersect(arg$items, names(level1))

  # Create weights from population targets
  if (!is.null(arg$targets)) {
    level1 <- create_weights(level1, arg)
  }

  arg$use_geo <- update_use_geo(level1, arg)
  arg$use_t <- set_use_t(level1, arg)

  level1 <- subset_to_estimation_periods(level1, arg)
  if (!is.null(arg$level2)) {
    arg$level2 <- subset_to_estimation_periods(arg$level2, arg)
  }

  # Create _gt. variables
  level1 <- create_gt_variables(d = level1, .items = arg$items)
  level1 <- drop_rows_missing_items(level1, arg)

  # Fix levels of variables after filtering
  level1 <- droplevels(level1)

  arg$level2 <- subset_to_observed_geo_periods(arg)

  # Quick hack: observed periods should be be the periods observed after
  # dropping observations
  if (is.factor(level1[[arg$time_id]])) {
    checks$observed_periods <- levels(droplevels(level1[[arg$time_id]]))
  } else {
    checks$observed_periods <- unique(level1[[arg$time_id]])
  }
  checks$observed_periods <- as.character(checks$observed_periods)

  # Represent covariates in data as model frame
  f0 <- as.formula(paste("~", paste("0", arg$geo_id, paste0(arg$demo_id, collapse = " + "), sep = " + ")))
  f0_t <- as.formula(paste("~", paste("0", arg$geo_id, paste0(arg$demo_id, collapse = " + "), arg$time_id, sep = " + ")))
  MF <- model.frame(f0, data = level1, na.action = return)
  MF_t <- model.frame(f0_t, data = level1, na.action = return)
  # Get frequency counts of factor combinations (reordered in factor order)
  xtab_t <- as.data.frame(table(MF_t))
  stopifnot(checks$observed_periods[1] %in% as.character(xtab_t[[arg$time_id]]))
  xtab <- xtab_t %>%
    dplyr::filter_(lazyeval::interp(~ x %in% y, x = as.name(arg$time_id),
      y = checks$observed_periods[1])) %>%
    dplyr::select_(.dots = c(arg$geo_id, arg$demo_id, arg$time_id))
  stopifnot(nrow(xtab) > 0)

  # Create a variable counting the number of responses given by each respondent in the data
  level1$n_responses <- count_respondent_trials(level1)

  ## GROUP LEVEL ##

  # Create table of design effects
  design_effects <- summarize_design_effects(level1, arg)
  group_grid <- make_group_grid(level1, arg)
  # Create table of (adjusted) trial counts by geographic, demographic, and time variable combination
  trial_counts <- summarize_trial_counts(level1, design_effects, group_grid, arg)
  # Create table of (weighted) average outcomes within combinations of geographic, demographic, and time variables.
  mean_y <- summarize_mean_y(level1, group_grid, arg)
  # Take the product of the (weighted) average cell outcome and the (adjusted) trial count
  success_counts <- summarize_success_count(trial_counts, mean_y, arg)
  # Create a summary table: for each period t, are there any nonmissing responses?
  trials_by_period <- summarize_trials_by_period(trial_counts, arg)
  # Giving us the periods in which we observe responses, which may be different from the periods we"ll use in estimation (given in arg$use_t)
  checks$nonmissing_t <- trials_by_period %>%
    dplyr::filter_(~valid_items == TRUE) %>%
    dplyr::select_(arg$time_id) %>%
    unlist(as.character(.))

  # NOTE: use_t must match nonmissing t at the moment

  # T gives the number of time periods
  T <- length(arg$use_t)
  # Q gives the number of questions
  Q <- count_questions(level1)
  # G gives the number of covariate combinations
  G <- count_covariate_combos(level1, arg)

  Gl2 <- count_level2_groups(arg$level2, xtab, arg)
  XX <- model.matrix(f0, xtab)
  dimnames(XX)[[1]] = tidyr::unite_(xtab, "rowname",
    c(arg$geo_id, arg$demo_id), sep = "_x_")$rowname

  # NOTE: not implemented
  # Calculate G x T x Gl2 weight matrix
  WT <- make_dummy_weight_matrix(T, Gl2, G)

  l2_only <- make_l2_only(trial_counts, level1, T, Q, arg)
  NNl2 <- make_NNl2(level1, T, Q, Gl2, arg)
  SSl2 <- NNl2

  ns_long_obs <- make_ns_long_obs(trial_counts, success_counts, arg)
  ns_long <- make_ns_long(ns_long_obs, arg)
  n_vec <- unlist(ns_long$n_grp)
  s_vec <- unlist(ns_long$s_grp)
  names(n_vec) <- ns_long$name
  names(s_vec) <- ns_long$name

  MMM <- make_missingness_array(ns_long, group_grid, arg)

  # The indicator matrix for hierarchical variables, XX, has as many columns as geographic units; drop one
  XX <- XX[, -1]

  # Assemble data for the geographic model
  ZZ <- create_l2_design_matrix(XX, arg)
  ZZ_prior <- create_l2_design_matrix(XX, arg)

  stan_data <- list(
    n_vec = n_vec,      # trial counts
    s_vec = s_vec,      # sums
    NNl2 = NNl2,
    SSl2 = SSl2,
    XX = XX,
    ZZ = ZZ,            # geographic predictors
    ZZ_prior = ZZ_prior,
    MMM = MMM,          # missingness array (T x Q x G)
    G = G,              # number of covariate groups
    Q = Q,              # number of questions (items)
    T = T,              # number of time units (years)
    N = length(n_vec),  # number of observed group-question cells
    P = ncol(XX),       # number of hierarchical parameters
    S = dim(ZZ)[[2]],   # number of geographic units
    H = dim(ZZ)[[3]],   # number of geographic-level predictors
    Hprior = dim(ZZ_prior)[[3]],
    separate_t = as.integer(arg$separate_periods),  # if 1, no pooling over time
    constant_item = as.integer(arg$constant_item),  # if 1, item parameters constant
    D = as.integer(arg$difficulty_count),           # number of difficulty parameters
    WT = WT,            # weight matrix for calculating level-two mean
    l2_only = l2_only,
    Gl2 = Gl2,          # number of second-level groups
    delta_tbar_prior_mean = arg$delta_tbar_prior_mean,
    delta_tbar_prior_sd = arg$delta_tbar_prior_sd,
    innov_sd_delta_scale = arg$innov_sd_delta_scale,
    innov_sd_theta_scale = arg$innov_sd_theta_scale)

  # Check dimensions against what Stan will expect
  check_dimensions(stan_data)

  return(stan_data)
}

as_tbl <- function(dataframe) {
  if (is.null(dataframe)) {
    return(NULL)
  } else {
    return(dplyr::as.tbl(dataframe))
  }
}

check_dimensions = function(stan_data) {
  with(stan_data, {
    stopifnot(identical(length(n_vec), length(s_vec)))
    stopifnot(identical(dim(NNl2), as.integer(c(T, Q, Gl2))))
    stopifnot(identical(dim(SSl2), as.integer(c(T, Q, Gl2))))
    stopifnot(identical(dim(MMM), c(T, Q, G)))
    stopifnot(identical(dim(WT), as.integer(c(T, Gl2, G))))
    stopifnot(identical(dim(l2_only), c(T, Q)))
    stopifnot(identical(dim(XX), c(G, P)))
    stopifnot(identical(dim(ZZ), c(T, P, H)))
    stopifnot(identical(dim(ZZ_prior), c(T, P, H)))
    stopifnot(!is.null(constant_item))
    stopifnot(!is.null(separate_t))
    })
}

check_restrictions <- function(level1, .arg) {
  checks <- list()
  # We need to find out which time periods are observed in the data
  checks$observed_periods <- get_t(level1[[.arg$time_id]])

  # And which question columns have no valid responses
  checks$q_all_missing <- get_missing_items(level1, .arg$items)

  # And which variables don"t satisfy the min_periods requirement
  checks$q_when_asked <- get_question_periods(level1, .arg)
  checks$q_rare.t <- get_rare_items_over_t(checks, .arg)

  # And which variables don"t satisfy the min_surveys requirement
  checks$q_which_asked <- get_question_polls(level1, .arg)
  checks$q_rare.polls <- get_rare_items_over_polls(checks, .arg)

  return(checks)
}

get_question_periods <- function(.data, .arg) {
  .data %>%
    dplyr::group_by_(.arg$time_id) %>%
    dplyr::summarise_each_(~anyValid, vars = .arg$items)
}

get_rare_items_over_t <- function(.checks, .arg) {
  stopifnot(is.integer(.arg$min_periods))
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

get_t <- function(t_data) {
  observed_t <- unlist(t_data)
  stopifnot(is.factor(observed_t))
  observed_t <- droplevels(observed_t)
  levels(observed_t)
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

summarize_design_effects <- function(.data, .arg) {
  stopifnot(is.numeric(.data[[.arg$survey_weight]]))
  de_table <- dplyr::as.tbl(.data) %>%
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id)) %>%
    dplyr::select_(.arg$survey_weight) %>%
    dplyr::summarise_("def" = lazyeval::interp(~create_design_effects(w), w = as.name(.arg$survey_weight))) %>%
    dplyr::arrange_(.arg$geo_id, .arg$demo_id, .arg$time_id)
  return(de_table)
}

summarize_trial_counts <- function(.data, design_effects, group_grid, .arg) {
  trial_counts <- .data %>%
    # The _gt variables can take values of 0/1/NA
    dplyr::mutate_each_(~notNA, ~matches("_gt\\d+$")) %>%
    # For a respondent who only answered questions A and B, we now have
    #  T,   T,   T,   T,   T,   F,   F.
    dplyr::mutate_each_(~. / n_responses, vars = ~matches("_gt\\d+$")) %>%
    # Dividing the booleans by n_responses = 5 calculated earlier gives us
    #.02, .02, .02, .02, .02,   0,   0.
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id)) %>%
    dplyr::summarise_each_(~sum, vars = ~matches("_gt\\d+$")) %>%
    dplyr::ungroup() %>%
    # Summing over respondents within the grouping variables gives the count
    # of trials for each combination of geo, demo, and time variables, divided
    # equally across the _gt indicators.
    # Joining the design effect table by the grouping variables attaches
    # the design effect associated with each variable combination.
    muffle_full_join(design_effects, by = c(.arg$geo_id, .arg$demo_id, .arg$time_id)) %>%
    dplyr::mutate_each_(~ceiling(. / def), vars = ~matches("_gt\\d+$")) %>%
    # Dividing by the design effect gives us a design-effect-adjusted trial
    # count.
    dplyr::select_(~-def) %>%
    # Tidy up.
    dplyr::mutate_each_(~as.character, vars = c(.arg$geo_id, .arg$demo_id, .arg$time_id))

  # A dplyr::full_join of trial_counts and group_grid will create rows for the desired
  # covariate combinations, where unobserved cells take NA values
  trial_counts <- muffle_full_join(trial_counts, group_grid,
    by = c(.arg$geo_id, .arg$demo_id, .arg$time_id))
  # Replace the NA values in those rows representing unobserved covariate
  # combinations with zeroes
  trial_counts <- trial_counts %>% dplyr::mutate_each_(~replaceNA, ~matches("_gt\\d+$"))
  # NOTE: Order will matter in a moment
  trial_counts <- trial_counts %>% dplyr::arrange_(.arg$geo_id, .arg$demo_id, .arg$time_id) %>%
    dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  return(trial_counts)
}

summarize_mean_y <- function(level1, group_grid, .arg) {
  mean_y <- level1 %>%
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id)) %>%
    dplyr::select_(~matches('_gt\\d+$'), ~n_responses, .arg$survey_weight) %>%
    dplyr::mutate_("weight" = lazyeval::interp(~ w / n,
      w = as.name(.arg$survey_weight), n = quote(n_responses))) %>%
    dplyr::summarise_each_(dplyr::funs(weighted.mean(as.vector(.), weight,
      na.rm = TRUE)), vars = "contains('_gt')") %>%
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id)) %>%
    dplyr::mutate_each(~replaceNaN) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_each_(~as.character, vars = c(.arg$geo_id, .arg$demo_id, .arg$time_id))

  # As above, create rows for unobserved but desired covariate combinations,
  # but this time leave the values as NA
  mean_y <- muffle_full_join(mean_y, group_grid,
    by = c(.arg$geo_id, .arg$demo_id, .arg$time_id)) %>%
      # NOTE: Order will matter in a moment
      dplyr::arrange_(.arg$geo_id, .arg$demo_id, .arg$time_id) %>%
      dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  return(mean_y)
}

summarize_success_count <- function(trial_counts, mean_y, .arg) {
  # Confirm row order is identical before taking product
  stopifnot(identical(
      dplyr::select_(mean_y, .dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id)),
      dplyr::select_(trial_counts, .dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id))))
  success_counts <- get_gt(trial_counts) * get_gt(mean_y)
  success_counts <- success_counts %>%
    # Reattach our identifiers
    dplyr::bind_cols(dplyr::select_(trial_counts, .dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id)), .) %>%
    # Round off returning integers and replace NA with 0
    dplyr::ungroup() %>%
    dplyr::mutate_each_(~replaceNA, ~matches("_gt\\d+$")) %>%
    dplyr::mutate_each_(~round(., digits = 0), ~matches("_gt\\d+$")) %>%
    dplyr::arrange_(.arg$geo_id, .arg$demo_id, .arg$time_id)
  return(success_counts)
}

summarize_trials_by_period <- function(trial_counts, .arg) {
  dplyr::select_(trial_counts, ~matches("_gt\\d+$"), .arg$time_id) %>%
    reshape2::melt(id.vars = .arg$time_id) %>%
    dplyr::group_by_(.arg$time_id) %>%
    dplyr::summarise_(valid_items = ~sum(value, na.rm = TRUE) > 0)
}

make_group_identifier <- function(.data, .arg) {
  interaction(.data[, .arg$groups], drop = TRUE)
}

make_ns_long_obs <- function(trial_counts, success_counts, .arg) {
  trial_counts_melt <- wrap_melt(trial_counts,
    id.vars = c(.arg$geo_id, .arg$demo_id, .arg$time_id), value.name = "n_grp")
  success_counts_melt <- wrap_melt(success_counts,
    id.vars = c(.arg$geo_id, .arg$demo_id, .arg$time_id), value.name = "s_grp")
  joined <- dplyr::left_join(trial_counts_melt, success_counts_melt,
      by = c(.arg$geo_id, .arg$demo_id, .arg$time_id, "variable")) %>%
    dplyr::arrange_(.dots = c("variable", .arg$time_id, .arg$demo_id, .arg$geo_id)) %>%
    tidyr::unite_("name", c(.arg$geo_id, .arg$demo_id), sep = "_x_", remove = FALSE) %>%
    dplyr::filter_(~n_grp != 0) %>%
    dplyr::as.tbl()
  return(joined)
}

wrap_melt <- function(...) {
  melt <- reshape2::melt(...)
  melt$variable <- as.character(melt$variable)
  return(melt)
}

make_ns_long <- function(ns_long_obs, .arg) {
  ns_long <- setNames(data.frame(expand.grid(unique(ns_long_obs$name), .arg$use_t)), c("name", .arg$time_id)) %>%
    dplyr::mutate_each(~as.character) %>%
    dplyr::left_join(ns_long_obs, ., by = c("name", .arg$time_id)) %>%
    dplyr::mutate_(
      n_grp = ~replaceNA(n_grp),
      name = ~paste(name, .arg$time_id, sep = "_x_"),
      name = ~paste(name, variable, sep = " | ")) %>%
    dplyr::arrange_(.arg$geo_id, .arg$demo_id, "variable", .arg$time_id)

  ns_long$s_grp[is.na(ns_long$s_grp)] <- 0
  ns_long$n_grp[is.na(ns_long$n_grp)] <- 0
  ns_long$s_grp <- as.integer(ns_long$s_grp)
  ns_long$n_grp <- as.integer(ns_long$n_grp)
  ns_long
}

make_missingness_array <- function(ns_long, group_grid, .arg) {
  # Create missingness indicator array
  acast_formula = as.formula(paste0(.arg$time_id, "~ variable ~", paste(.arg$demo_id, collapse = "+"), "+", .arg$geo_id))
  MMM <- ns_long %>%
    # Include in the missingness array all group-variables that might not exist
    # in the data because of unobserved use_t
    muffle_full_join(group_grid, by = c(.arg$geo_id, .arg$demo_id, .arg$time_id)) %>%
    # Get missingness by group
    dplyr::group_by_(.dots = c(.arg$geo_id, .arg$demo_id, .arg$time_id, "variable")) %>%
    dplyr::summarise_("m_grp" = ~as.integer(sum(n_grp, na.rm = TRUE) == 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~paste0("x_", geo), geo = as.name(.arg$geo_id))), .arg$geo_id)) %>%
    reshape2::acast(acast_formula, value.var = "m_grp", fill = 1)

  # But we don"t want to include "NA" as a variable
  MMM <- MMM[!(dimnames(MMM)[[1]] == "NA"),
    !(dimnames(MMM)[[2]] == "NA"),
    !(dimnames(MMM)[[3]] == "NA")]
  # No cells should be NA either
  if (any(is.na(MMM))) {
    stop("No cell of MMM should be NA")
  }
  MMM
}

make_dummy_weight_matrix <- function(T, Gl2, G) {
  array(1, dim = c(T, Gl2, G))
}

make_l2_only <- function(trial_counts, level1, T, Q, .arg) {
  l2_only <- array(0, dim = c(T, Q), dimnames = list(.arg$use_t,
      grep("_gt", colnames(level1), fixed = TRUE, value = TRUE)))
  l2_only <- trial_counts %>%
    dplyr::group_by_(.arg$time_id) %>%
    dplyr::summarise_each_(dplyr::funs(c(0)), ~contains("_gt"))
  rownames(l2_only) <- l2_only[[.arg$time_id]]
  l2_only <- l2_only %>%
    dplyr::select_(~contains("_gt"))
  as.matrix(l2_only, rownames.force = TRUE)
}

make_NNl2 <- function(level1, T, Q, Gl2, .arg) {
  array(0, c(T, Q, Gl2), list(.arg$use_t,
      grep("_gt", colnames(level1), fixed= TRUE, value = TRUE), NULL))
}

factorize_arg_vars <- function(.data, .arg) {
  if (is.null(.data)) {
    return(NULL)
  }
  arg_vars <- intersect(names(.data),
    c(.arg$time_id, .arg$groups, .arg$geo_id, .arg$survey_id))
  is_numeric_group = apply(.data[, .arg$groups], 2, class) == "numeric"
  if (any(is_numeric_group)) {
    message("Defining groups via numeric variables is allowed, but output names won't be descriptive. Consider using factors.")
    for (varname in .arg$groups[is_numeric_group]) {
      .data[[varname]] = paste0(varname, as.character(.data[[varname]]))
    }
  }
  .data %>% dplyr::mutate_each_(dplyr::funs(factor), vars = arg_vars)
}

drop_missing_respondents <- function(.data, .arg) {
  .data <- .data %>% dplyr::filter_(lazyeval::interp(~!none_valid))
  if (!.arg$silent) {
    message(sprintf(ngettext(sum(.data$none_valid),
          "%i row has no responses",
          "%i rows have no responses"),
        sum(.data$none_valid)))
  }
  .data
}

drop_missing_items <- function(.data, q_all_missing, .arg) {
  q_all_missing <- intersect(q_all_missing, names(.data))
  if (length(q_all_missing) > 0) {
    .data <- .data %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = q_all_missing))
  }
  if (!.arg$silent) {
    message(sprintf(ngettext(length(q_all_missing),
          "%i question, %s, has no responses",
          "%i questions have no responses: %s"),
        length(q_all_missing),
        stringi::stri_c(q_all_missing, collapse = ", ")))
  }
  .data
}

drop_rare_items_over_t <- function(.data, q_rare, .arg) {
  q_rare <- intersect(q_rare, names(.data))
  if (length(q_rare) > 0) {
    .data <- .data %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = q_rare))
  }
  if (!.arg$silent) {
    message(sprintf(ngettext(length(q_rare),
          "%i question fails min_periods requirement (%i): %s",
          "%i questions fail min_periods requirement (%i): %s"),
        length(q_rare), .arg$min_periods,
        stringi::stri_c(q_rare, collapse = ", ")))
  }
  .data
}

drop_rare_items_over_polls <- function(.data, .lt_min_surveys, .arg) {
  .lt_min_surveys <- intersect(.lt_min_surveys, names(.data))
  if (length(.lt_min_surveys) > 0) {
    .data <- .data %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = .lt_min_surveys))
    if (!.arg$silent) {
    }
    message(sprintf(ngettext(length(.lt_min_surveys),
          "%i question fails min_surveys requirement (%i): %s",
          "%i questions fail min_surveys requirement (%i): %s"),
        length(.lt_min_surveys), .arg$min_surveys,
        stringi::stri_c(.lt_min_surveys, collapse = ", ")))
  }
  .data
}

update_use_geo <- function(.data, .arg) {
  levels(unlist(.data[[.arg$geo_id]]))
}

set_use_t <- function(.data, .arg) {
  if (is.null(.arg$use_t)) {
    return(levels(.data[[.arg$time_id]]))
  } else {
    return(.arg$use_t)
  }
}

subset_to_estimation_periods <- function(.data, .arg) {
  stopifnot(is.data.frame(.data))
  stopifnot(is.factor(.data[[.arg$time_id]]))
  stopifnot(is.character(.arg$use_t))
  periods_filter <- as.character(.data[[.arg$time_id]]) %in%
    as.character(.arg$use_t)
  .data <- .data %>% dplyr::filter(periods_filter)
  stopifnot(nrow(.data) > 0)
  .data <- droplevels(.data)
  .data
}

drop_rows_missing_covariates <- function(.data, .arg) {
  .data <- .data %>%
    dplyr::filter_(lazyeval::interp(~!is.na(geo_name) & !is.na(time_name),
      geo_name = as.name(.arg$geo_id),
      time_name = as.name(.arg$time_id)))
  for (v in .arg$demo_id) {
    .data <- .data %>%
      dplyr::filter_(lazyeval::interp(~!is.na(varname), varname = as.name(v)))
  }
  .data <- droplevels(.data)
  return(.data)
}

drop_rows_missing_items <- function(.data, .arg) {
  # Respondents should have at least one item response
  item_filter = rowSums(!is.na(.data[, .arg$items])) > 0
  .data = .data %>% dplyr::filter(item_filter)
  droplevels(.data)
}

subset_to_observed_geo_periods <- function(.arg) {
  # Subset level-2 data to geographic units observed and time periods
  # specified
  if (is.null(.arg$level2)) {
    return(NULL)
  } else {
    .arg$level2 <- .arg$level2 %>%
      dplyr::filter_(lazyeval::interp(~geo %in% .arg$use_geo,
        geo = as.name(.arg$geo_id)))
    stopifnot(nrow(.arg$level2) > 0)
  }
  .arg$level2[[.arg$geo_id]] <- droplevels(.arg$level2[[.arg$geo_id]])
  return(.arg$level2)
}

get_gt = function(level1) {
  gts = level1 %>% dplyr::select_(lazyeval::interp(~matches(x), x = "_gt\\d+$"))
  stopifnot(ncol(gts) > 0)
  return(gts)
}

count_respondent_trials <- function(level1) {
  gts = get_gt(level1)
  rowSums(!is.na(as.matrix(gts)), na.rm = TRUE)
}

count_questions <- function(level1) {
  sum(grepl("_gt", colnames(level1), fixed = TRUE))
}

count_covariate_combos <- function(level1, .arg) {
  factor_levels = nlevels_vectorized(level1, c(.arg$geo_id, .arg$demo_id))
  Reduce(`*`, factor_levels)
}

nlevels_vectorized = function(data, varlist) {
  sapply(data[varlist], nlevels)
}

add_survey_period_id <- function(level1, .arg) {
  level1 <- level1 %>% tidyr::unite_("survey_period_id",
    c(.arg$survey_id, .arg$time_id), sep = "_x_", remove = FALSE)
  level1 <- level1 %>% dplyr::mutate_(.dots =
    setNames(list(~as.factor(survey_period_id)), "survey_period_id"))
}

handle_data <- function(.data, .arg) {
  .data <- as_tbl(.data)
  # Make all the variables given as strings factors
  .data <- factorize_arg_vars(.data, .arg)
  return(.data)
}

apply_restrictions <- function(.data, .checks, .arg) {
  # Drop rows with no valid question responses
  .data <- drop_missing_respondents(.data, .arg)

  # Drop variables columns with no valid responses
  .data <- drop_missing_items(.data, .checks$q_all_missing, .arg)

  # Drop variables that don"t satisfy min_periods requirement
  .data <- drop_rare_items_over_t(.data, .checks$q_rare.t, .arg)

  if (length(.arg$items) == 0) stop("no response variables left")
  if (nrow(.data) == 0) stop("no rows left")

  .data <- add_survey_period_id(.data, .arg)

  .data <- drop_rare_items_over_polls(.data, .checks$q_rare.polls, .arg)

  return(.data)
}

count_level2_groups <- function(level2, xtab, .arg) {
  # Generate factor levels for combinations of demographic variables
  # Gl2 gives the number of level-2 modifier combinations
  if (is.null(.arg$level2)) {
    l2.group <- gl(1, nrow(xtab))
    Gl2 <- nlevels(l2.group)
    return(Gl2)
  } else {
    Gl2 <- nlevels_vectorized(.arg$level2, .arg$demo_id)
    if (Gl2 == 0) Gl2 <- 1
    stopifnot(Gl2 > 0)
    return(Gl2)
  }
}

make_group_grid <- function(level1, .arg) {
  # The table just created excludes covariate combinations not observed in the
  # data, but we want all combinations of the demographic and geographic
  # covariates and specified periods
  expand.grid(c(setNames(list(as.character(.arg$use_t)), .arg$time_id),
    lapply(level1[, c(.arg$geo_id, .arg$demo_id)], function(x) sort(unique(x)))))
}

muffle_full_join <- function(...) {
  suppressWarnings(dplyr::full_join(...))
}
