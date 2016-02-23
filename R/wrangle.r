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

data(state_opinion)
data = list(level1 = state_opinion)
vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
            groups = c("race"),
            time_id = "year",
            geo_id = "state",
            survey_id = "source",
            survey_weight = "weight")
filters = list(periods = c(2006:2010))

# wrangle(data = data, vars = vars, filters = filters)

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
    stan_data <- shape(item)
    stan_data
}

make_hierarchical_array <- function(item) {
  if (item$has_hierarchy()) {
    ZZ <- shape_hierarchical_data(item)
  } else {
    zz.names <- list(item$filters$t, dimnames(item$modifier$group_design_matrix)[[2]], "")
    ZZ <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  }
  ZZ
}

shape_hierarchical_data <- function(item) {
  # the array of hierarchical data ZZ should be T x P x H, where T is the number of time periods, P is the number of
  # hierarchical parameters (including the geographic), and H is the number of predictors for geographic unit effects
  # TODO: make flexible; as written we must model geo x t
  modeled_params = c(item$geo, item$time)
  unmodeled_params = setdiff(c(item$geo, item$time, item$groups), modeled_params)
  # TODO: uniqueness checks on level2 data (state_demographics should not pass)
  # TODO: confirm sort order of level2_modifier
  missing_modifier_geo <- setdiff(
    unique(unlist(dplyr::select_(item$group_grid_t, item$geo))),
    unique(unlist(dplyr::select_(item$modifier$tbl, item$modifier$geo))))
  if (length(missing_modifier_geo) > 0) stop("no hierarchical data for geo in item data: ", missing_modifier_geo)
  missing_modifier_t <- setdiff(item$filters$t, unlist(dplyr::select_(item$modifier$tbl, item$time)))
  if (length(missing_modifier_t) > 0) stop("missing hierarchical data for t in item data: ", missing_modifier_t)

  hier_frame <- item$modifier$tbl %>% dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~paste0(item$geo, geo),
      geo = as.name(item$geo))), item$geo)) %>%
    dplyr::select_(.dots = c(modeled_params, item$modifier$modifiers)) %>%
    dplyr::rename_("param" = item$geo) %>%
    dplyr::mutate_("param" = ~as.character(param)) %>%
    dplyr::arrange_(.dots = c("param", item$time))

  modeled_param_names <- unique(unlist(dplyr::select_(hier_frame, "param")))
  unmodeled_param_levels = lapply(unmodeled_params, function(x) {
      paste0(x, levels(item$group_grid_t[[x]]))[-1]
    }) %>% unlist()
  param_levels <- c(modeled_param_names, unmodeled_param_levels)

  unmodeled_frame <- expand.grid(c(list(
      unmodeled_param_levels, 
      sort(unique(unlist(dplyr::select_(item$modifier$tbl, item$modifier$time))))),
      rep(list(0), length(item$modifier$modifiers)))) %>%
    setNames(c("param", item$time, item$modifier$modifiers)) %>%
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
    dplyr::mutate_each_(~ifelse(is.na(.), 0, .), vars = item$modifier$modifiers)
  hier_melt <- wrap_melt(hier_frame, id.vars = c("param", item$time), variable.name = "modifiers") %>%
    dplyr::mutate_("param" = ~factor(param, levels = param_levels, ordered = TRUE))
  if (!inherits(hier_melt$value, "numeric")) stop("non-numeric values in hierarchical data. Factor handling probably failed. Possible quickfix: omit or manually dummy out any factors in modifiers.")
  assertthat::assert_that(names_subset(modeled_param_names, unlist(hier_melt$param)))
  assertthat::assert_that(names_subset(unmodeled_param_levels, unlist(hier_melt$param)))
  melt_formula <- as.formula(paste(item$time, "param", "modifiers", sep = " ~ "))
  zz <- reshape2::acast(hier_melt, melt_formula, drop = FALSE, value.var = "value")
  zz <- zz[, -1, , drop = FALSE]
  zz
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

get_question_periods <- function(item) {
  question_periods <- item$tbl %>%
    dplyr::group_by_(item$time) %>%
    dplyr::summarise_each_(~any_not_na, vars = item$items)
  question_periods
}

make_design_effects <- function(item) {
  de_table <- item$tbl %>%
    dplyr::group_by_(.dots = c(item$geo, item$groups, item$time)) %>%
    dplyr::select_(item$weight)
  de_table <- de_table %>%
    dplyr::summarise_("def" = lazyeval::interp(
      ~calc_design_effects(w), w = as.name(item$weight))) %>%
    dplyr::arrange_(.dots = c(item$geo, item$groups, item$time))

  assertthat::assert_that(has_all_names(de_table, c("def", item$geo, item$groups, item$time)))
  de_table
}

count_trials <- function(item, design_effects) {
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
    dplyr::ungroup()
    # Summing over respondents within the grouping variables gives the count
    # of trials for each combination of geo, demo, and time variables, divided
    # equally across the _gt indicators.
    # Joining the design effect table by the grouping variables attaches
    # the design effect associated with each variable combination.
  trial_counts <- trial_counts %>%
    muffle_full_join(design_effects, by = c(item$geo, item$groups, item$time)) %>%
    dplyr::mutate_each_(~ceiling(. / def), vars = ~matches("_gt\\d+$")) %>%
    # Dividing by the design effect gives us a design-effect-adjusted trial
    # count.
    dplyr::select_(~-def)

  # A dplyr::full_join of trial_counts and group_grid will create rows for the desired
  # covariate combinations, where unobserved cells take NA values
  trial_counts <- muffle_full_join(trial_counts, item$group_grid,
    by = c(item$geo, item$groups, item$time))
  # Replace the NA values in those rows representing unobserved covariate
  # combinations with zeroes
  trial_counts <- trial_counts %>% dplyr::mutate_each_(~na_to_zero, ~matches("_gt\\d+$"))
  # NOTE: Order will matter in a moment
  trial_counts <- trial_counts %>%
    dplyr::arrange_(.dots = c(item$geo, item$groups, item$time)) %>%
    dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  trial_counts
}

make_group_means <- function(item) {
  weighted_responses <- item$tbl %>%
    dplyr::group_by_(.dots = c(item$geo, item$groups, item$time)) %>%
    # subset to identifiers and items
    dplyr::select_(~matches("_gt\\d+$"), ~n_responses, item$weight) %>%
    # weight by n_responses
    dplyr::mutate_("weight" = lazyeval::interp(~ w / n,
      w = as.name(item$weight), n = quote(n_responses)))
    # take weighted mean of item responses within geo, group, time
  mean_y <- weighted_responses %>%
    dplyr::summarise_each_(~weighted.mean(as.vector(.), weight,
      na.rm = TRUE), vars = "contains(\"_gt\")") %>%
    dplyr::group_by_(.dots = c(item$geo, item$groups, item$time)) %>%
    # replace NaN
    dplyr::mutate_each(~nan_to_na) %>%
    dplyr::ungroup()

  # make sure missing group appear as NA
  mean_y <- muffle_full_join(mean_y, item$group_grid,
    by = c(item$geo, item$groups, item$time)) %>%
      # NOTE: Order will matter in a moment
      dplyr::arrange_(.dots = c(item$geo, item$groups, item$time)) %>%
      dplyr::mutate_each_(~as.vector, ~matches("_gt\\d+$"))
  mean_y
}

count_successes <- function(trial_counts, mean_group_outcome) {
  # Confirm row order is identical before taking product
  assertthat::assert_that(all_equal(
      dplyr::select_(mean_group_outcome, .dots = c(item$geo, item$groups, item$time)),
      dplyr::select_(trial_counts, .dots = c(item$geo, item$groups, item$time))))
  success_counts <- get_gt(trial_counts) * get_gt(mean_group_outcome)
  success_counts <- success_counts %>%
    # Reattach our identifiers
    dplyr::bind_cols(dplyr::select_(trial_counts, .dots = c(item$geo, item$groups, item$time)), .) %>%
    # Round off returning integers and replace NA with 0
    dplyr::ungroup() %>%
    dplyr::mutate_each_(~na_to_zero, ~matches("_gt\\d+$")) %>%
    dplyr::mutate_each_(~round(., digits = 0), ~matches("_gt\\d+$")) %>%
    dplyr::arrange_(.dots = c(item$geo, item$groups, item$time))
  success_counts
}

make_design_matrix <- function(item) {
  design_formula <- as.formula(paste("~ 0", item$geo, paste(item$groups, collapse = " + "), sep = " + "))
  design_matrix <- with_contr.treatment(model.matrix(design_formula, item$group_grid_t))
  design_matrix <- cbind(design_matrix, item$group_grid_t) %>%
    dplyr::arrange_(.dots = c(item$groups, item$geo))
  group_names <- concat_groups(item$group_grid_t, item$groups, item$geo, "name")
  design_matrix <- concat_groups(design_matrix, item$groups, item$geo, "name")
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

summarize_trials_by_period <- function(trial_counts) {
  dplyr::select_(trial_counts, ~matches("_gt\\d+$"), item$time) %>%
    reshape2::melt(id.vars = item$time) %>%
    dplyr::group_by_(item$time) %>%
    dplyr::summarise_(valid_items = ~sum(value, na.rm = TRUE) > 0)
}

format_counts <- function(trial_counts, success_counts) {
  trial_counts_melt <- wrap_melt(trial_counts, variable.name = "item",
    id.vars = c(item$geo, item$groups, item$time), value.name = "n_grp")
  success_counts_melt <- wrap_melt(success_counts, variable.name = "item",
    id.vars = c(item$geo, item$groups, item$time), value.name = "s_grp")
  joined <- dplyr::left_join(trial_counts_melt, success_counts_melt,
      by = c(item$geo, item$groups, item$time, "item")) %>%
    tidyr::unite_("name", c(item$geo, item$groups), sep = "__", remove = FALSE) %>%
    dplyr::filter_(~n_grp != 0) %>%
    dplyr::mutate_(
      n_grp = ~na_to_zero(n_grp),
      s_grp = ~na_to_zero(s_grp),
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

get_missingness <- function(item) {
 item$group_counts %>%
    # Include in the missingness array all group-variables that might not exist
    # in the data because of unobserved use_t
    muffle_full_join(item$group_grid, by = c(item$geo, item$groups, item$time)) %>%
    # Get missingness by group
    dplyr::group_by_(.dots = c("item", item$time, item$groups, item$geo)) %>%
    dplyr::summarise_("m_grp" = ~as.integer(sum(n_grp, na.rm = TRUE) == 0)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(.dots = c(item$time, "item", item$groups, item$geo))
}

cast_missingness <- function(missingness) {
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

make_dummy_l2_only <- function(item) {
  gt_names <- grep("_gt\\d+$", colnames(item$tbl), value = TRUE)
  l2_only <- matrix(0L,
    nrow = length(item$filters$t),
    ncol = length(gt_names),
    dimnames = list(item$filters$t, gt_names))
  l2_only
}

make_dummy_l2_counts <- function(item) {
  array(0, c(item$T, item$Q, item$modifier$Gl2), list(item$filters$t,
      grep("_gt", colnames(item$tbl), fixed= TRUE, value = TRUE), item$modifier$modifiers))
}

drop_rows_missing_items <- function(item) {
  item_filter <- rowSums(!is.na(item$tbl[, item$items, drop = FALSE])) > 0
  item$tbl <- item$tbl %>% dplyr::filter(item_filter)
  item$tbl <- droplevels(item$tbl)
  item
}

get_gt <- function(tbl) {
  gts <- tbl %>% dplyr::select_(lazyeval::interp(~matches(x), x = "_gt\\d+$"))
  return(gts)
}

count_responses <- function(item) {
  gts <- get_gt(item$tbl)
  rowSums(!is.na(as.matrix(gts)), na.rm = TRUE)
}

count_modifier_groups <- function(item) {
  if (!item$has_hierarchy()) {
    hierarchical_group <- gl(1L, item$G)
    Gl2 <- nlevels(hierarchical_group)
  } else {
    Gl2 <- length(item$modifier$modifiers)
    Gl2 <- max(unlist(Gl2), 1L)
  }
}

muffle_full_join <- function(...) {
  suppressWarnings(dplyr::full_join(...))
}

calc_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
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

tostan <- function(item) {
  list(
    n_vec = setNames(item$group_counts$n_grp, item$group_counts$name),
    s_vec = setNames(item$group_counts$s_grp, item$group_counts$name),
    NNl2 = item$modifier$NNl2,
    SSl2 = item$modifier$SSl2,
    XX = item$modifier$group_design_matrix,
    ZZ = item$modifier$ZZ,            # geographic predictors
    ZZ_prior = item$modifier$ZZ_prior,
    MMM = item$MMM,          # missingness array (T x Q x G)
    G = item$G,              # number of covariate groups
    Q = item$Q,              # number of questions (items)
    T = item$T,              # number of time units (years)
    N = item$N,  # number of observed group-question cells
    P = item$modifier$P,       # number of hierarchical parameters
    S = item$modifier$S,   # number of geographic units
    H = item$modifier$H,   # number of geographic-level predictors
    Hprior = item$modifier$H_prior,
    separate_t = item$control$separate_t,  # if 1, no pooling over time
    constant_item = item$control$constant_item,  # if 1, item parameters constant
    D = ifelse(item$control$constant_item, 1L, item$T),           # number of difficulty parameters
    WT = item$modifier$WT,            # weight matrix for calculating level-two mean
    l2_only = item$modifier$l2_only,
    Gl2 = item$modifier$Gl2,          # number of second-level groups
    delta_tbar_prior_mean = item$control$delta_tbar_prior_mean,
    delta_tbar_prior_sd = item$control$delta_tbar_prior_sd,
    innov_sd_delta_scale = item$control$innov_sd_delta_scale,
    innov_sd_theta_scale = item$control$innov_sd_theta_scale,
    group_counts = item$group_counts,
    vars = list(items = item$items,
      gt_items = grep("_gt\\d+$", colnames(item$tbl), value = TRUE),
      groups = item$groups,
      time_id = item$time,
      use_t = item$filters$t,
      geo_id = item$geo,
      periods = arg$periods,
      survey_id = item$survey,
      covariate_groups = item$group_grid_t,
      hier_names = dimnames(item$modifier$group_design_matrix)[[2]]))
}
