shape <- function(item, control = list(...)) {
  item$restrict()
  item$reweight()
  item$make_gt_variables()
  item$list_groups()
  item$list_groups_t()
  item$group_n()
  item$find_missingness()
  item$make_WT()
  item$make_l2_only()
  item$make_NNl2()
  item$make_SSl2()
  item$make_group_design()
  item$make_ZZ()
  item$make_ZZ_prior()
  stan_data <- tostan(item)
  check(stan_data)
  stan_data
}

check <- function(stan_data) {
  check_dimensions(stan_data)
  check_values(stan_data)
  check_order(stan_data)
}

make_hierarchical_array <- function(item) {
  if (item$has_hierarchy) {
    ZZ <- shape_hierarchical_data(item)
  } else {
    zz.names <- list(item$filters$time, dimnames(item$modifier$group_design_matrix)[[2]], "")
    ZZ <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  }
  ZZ
}

create_gt_variables <- function(item){
  widths <- c("item" = 30, "class" = 10, "levels" = 12, "responses" = 16)
  print_varinfo_header(widths)
  out <- lapply(item$items, function(i) {
    if (is.ordered(item$tbl[[i]])) {
      i_levels <- na.omit(levels(droplevels(item$tbl[[i]])))
      values <- match(as.character(item$tbl[[i]]), i_levels)
    } else if (is.numeric(item$tbl[[i]])) {
      i_levels <- sort.int(na.omit(unique.default(item$tbl[[i]])))
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
  message()
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

make_group_grid <- function(item) {
  group_grid <- expand.grid(c(
    setNames(list(item$filters$time), item$time),
    lapply(item$tbl[, c(item$geo, item$groups), drop = FALSE], function(x) sort(unique(x)))))
  assertthat::assert_that(not_empty(group_grid))
  group_grid %>% dplyr::arrange_(.dots = c(item$time, item$groups, item$geo))
}

make_group_grid_t <- function(item) {
  group_grid_t <- item$group_grid %>%
    dplyr::select_(~-one_of(item$time)) %>%
    dplyr::distinct() %>%
    dplyr::arrange_(.dots = c(item$groups, item$geo))
  item$check_groups(group_grid_t)
  group_grid_t
}

restrict_items <- function(item) {
  item <- arrange_item_factors(item)
  item <- rename_numeric_groups(item)
  item <- arrange_item_factors(item)
  initial_dim <- dim(item$tbl)
  final_dim <- c()
  iter <- 1L
  while (!identical(initial_dim, final_dim)) {
    message()
    message("Applying restrictions, pass ", iter, "...")
    if (identical(iter, 1L)) item <- drop_rows_missing_covariates(item)
    initial_dim <- dim(item$tbl)
    item <- keep_t(item)
    item <- drop_itemless_respondents(item)
    item <- drop_responseless_items(item)
    item <- drop_items_rare_in_time(item)
    item <- drop_items_rare_in_polls(item)
    not_empty(item$tbl)
    final_dim <- dim(item$tbl)
    iter <- iter + 1L
    if (identical(initial_dim, final_dim)) {
      message("\tNo changes")
    } else {
      message("Remaining: ", format(nrow(item$tbl), big.mark = ","), " rows, ", length(item$items), " items")
    }
  }
  item
}

rename_numeric_groups <- function(item) {
  arg_vars <- intersect(names(item$tbl), c(item$groups, item$geo, item$modifier$modifiers, item$modifier$t1_modifiers, item$modifier$geo))
  numeric_groups <- item$tbl %>% dplyr::summarize_each_(~is.numeric, vars = arg_vars) %>% unlist()
  if (any(numeric_groups)) {
    message("Defining groups via numeric variables is allowed, but output names won't be descriptive. Consider using factors.")
    for (varname in names(numeric_groups)) {
      item$tbl[[varname]] <- paste0(varname, as.character(item$tbl[[varname]]))
    }
  }
  item
}

arrange_item_factors <- function(item) {
  vars <- intersect(names(item$tbl), c(item$groups, item$geo, item$survey))
  item$tbl <- arrange_factors(item$tbl, vars)
  item
}

arrange_modifier_factors <- function(item) {
  vars <- intersect(names(item$modifier$tbl), c(item$modifier$geo))
  item$modifier$tbl <- arrange_factors(item$modifier$tbl, vars)
  item
}

arrange_factors <- function(tbl, vars) {
  with_contr.treatment(tbl %>%
    dplyr::arrange_(.dots = vars) %>%
    dplyr::mutate_each_(dplyr::funs(factor(., levels = sort(unique(as.character(.))))), vars = vars))
}

drop_rows_missing_covariates <- function(item) {
  n <- nrow(item$tbl)
  item$tbl <- item$tbl %>%
    dplyr::filter_(lazyeval::interp(~!is.na(geo_name) & !is.na(time_name),
  geo_name = as.name(item$geo),
  time_name = as.name(item$time)))
  for (v in c(item$time, item$geo, item$groups, item$survey)) {
    item$tbl <- item$tbl %>%
      dplyr::filter_(lazyeval::interp(~!is.na(varname), varname = as.name(v)))
  }
  if (!identical(n, nrow(item$tbl))) {
    message("\tDropped ", format(n - nrow(item$tbl), big.mark = ","), " rows for missingness in covariates")
    item$tbl <- droplevels(item$tbl)
  }
  item
}

with_contr.treatment <- function(...) {
  contrast_options = getOption("contrasts")
  options("contrasts"= c(unordered = "contr.treatment", ordered = "contr.treatment"))
  res <- eval(...)
  options("contrasts"= contrast_options)
  res
}

keep_t <- function(item) {
  item$tbl <- item$tbl %>%
    dplyr::filter_(lazyeval::interp(~observed_t %in% keep_t, observed_t = as.name(item$time), keep_t = item$filters$time))
  not_empty(item$tbl)
  item
}

# item = items
drop_itemless_respondents <- function(item) {
  item$items <- intersect(item$items, colnames(item$tbl))
  item$tbl$none_valid <- get_itemless_respondents(item)
  if (any(item$tbl$none_valid)) {
    item$tbl <- item$tbl %>% dplyr::filter_(lazyeval::interp(~!none_valid))
    message(sprintf(ngettext(sum(item$tbl$none_valid),
          "\tDropped  %i row for lack of item responses",
          "\tDropped %i rows for lack of item responses"),
        format(sum(item$tbl$none_valid), big.mark = ",")))
  }
  item
}

drop_responseless_items <- function(item) {
  responseless_items <- get_responseless_items(item)
  item$items <- setdiff(item$items, responseless_items)
  if (length(responseless_items) > 0) {
    item$tbl <- item$tbl %>%
      dplyr::select_(~-one_of(responseless_items))
    message(sprintf(ngettext(length(responseless_items),
          "\tDropped %i item for lack of responses",
          "\tDropped %i items for lack of responses"),
        length(responseless_items)))
  }
  item
}

drop_items_rare_in_time <- function(item) {
  q_rare <- get_items_rare_in_time(item)
  item$items <- setdiff(item$items, q_rare)
  if (length(q_rare) > 0) {
    item$tbl <- item$tbl %>% dplyr::select_(.dots =  q_rare)
    message(sprintf(ngettext(length(q_rare),
          "\tDropped %i items for failing min_t requirement (%i)",
          "\tDropped %i items for failing min_t requirement (%i)"),
        length(q_rare), item$filters$min_t))
  }
  item
}

get_responseless_items <- function(item) {
  n_item_responses <- item$tbl %>%
    dplyr::summarise_each_(~sum(!is.na(.)), vars = item$items) %>%
    wrap_melt(id.vars = NULL, value.name = "n_responses")
  responseless_items <- n_item_responses %>%
    dplyr::filter_(~n_responses == 0L)
  responseless_items <- unlist(responseless_items$variable)
  assertthat::assert_that(is.character(responseless_items))
  responseless_items
}

get_itemless_respondents <- function(item) {
  if (length(item$items) < 1) stop("no remaining items")
  not_na <- !is.na(item$tbl %>%
    dplyr::select_(.dots = item$items))
  rowSums(not_na) == 0
}

get_items_rare_in_time <- function(item) {
  q_when_asked <- get_question_periods(item)
  q_t_count <- colSums(dplyr::select_(q_when_asked, ~-one_of(item$time)))
  q_rare <- names(q_t_count)[q_t_count < item$filters$min_t]
  q_rare
}

drop_items_rare_in_polls <- function(item) {
  lt_min_surveys <- get_items_rare_in_polls(item)
  item$items <- setdiff(item$items, lt_min_surveys)
  if (length(lt_min_surveys) > 0) {
    item$tbl <- item$tbl %>%
      dplyr::select_(~-one_of(lt_min_surveys))
  }
  assertthat::assert_that(assertthat::not_empty(setdiff(item$items, lt_min_surveys)))
  item
}

get_items_rare_in_polls <- function(item) {
  q_which_asked <- get_question_polls(item)
  q_counts <- q_which_asked %>% dplyr::select_(~-one_of(item$survey)) %>%
    dplyr::summarise_each(~sum)
  lt_min_surveys <- colnames(q_counts)[unlist(q_counts) < item$filters$min_survey]
  if (length(lt_min_surveys) > 0) {
  message(sprintf(ngettext(length(lt_min_surveys),
        "\t%i question fails min_surveys requirement (%i)",
        "\t%i questions fail min_surveys requirement (%i)"),
      length(lt_min_surveys), item$filters$min_survey))
  }
  lt_min_surveys
}

get_question_polls <- function(item) {
  question_polls <- item$tbl %>%
    dplyr::group_by_(item$survey) %>%
    dplyr::summarise_each_(dplyr::funs(any_not_na), vars = item$items)
  question_polls
}

get_question_periods <- function(item) {
  question_periods <- item$tbl %>%
    dplyr::group_by_(item$time) %>%
    dplyr::summarise_each_(~any_not_na, vars = item$items)
  question_periods
}

make_group_counts <- function(item) {
  item$tbl$n_responses <- count_responses(item)
  design_effects <- make_design_effects(item)
  trial_counts <- count_trials(item, design_effects)
  group_means <- make_group_means(item)
  success_counts <- count_successes(item, trial_counts, group_means)
  format_counts(item, trial_counts, success_counts)
}

make_missingness_array <- function(item) {
  missingness <- get_missingness(item)
  cast_missingness(item, missingness)
}

restrict_modifier <- function(item) {
  if (item$has_hierarchy()) {
    message()
    message("Restricting modifier data to time and geo observed in item data...")
    item <- arrange_modifier_factors(item)
    initial_nrow <- nrow(item$modifier$tbl)
    item$modifier$tbl <- item$modifier$tbl %>%
      dplyr::filter_(lazyeval::interp(~modifier_geo %in% item$filters$geo & modifier_t %in% filter_t,
        modifier_geo = as.name(item$modifier$geo),
        modifier_t = as.name(item$modifier$time),
        filter_t = item$filters$time))
    final_nrow <- nrow(item$modifier$tbl)
    message("\tDropped ", initial_nrow - final_nrow, " rows")
    message("Remaining: ", format(final_nrow, big.mark = ","), " rows")
    distinct_nrow <- item$modifier$tbl %>%
      dplyr::distinct_(item$modifier$modifiers, item$modifier$geo, item$modifier$time) %>%
      nrow()
    if (!identical(final_nrow, distinct_nrow)) stop("time and geo identifiers don't uniquely identify modifier data observations")
  }
  item
}

make_hierarchical_array <- function(item) {
  if (item$has_hierarchy()) {
    ZZ <- shape_hierarchical_data(item)
  } else {
    zz.names <- list(item$filters$time, dimnames(item$modifier$group_design_matrix)[[2]], "")
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
  missing_modifier_t <- setdiff(item$filters$time, unlist(dplyr::select_(item$modifier$tbl, item$time)))
  if (length(missing_modifier_t) > 0) stop("missing hierarchical data for t in item data: ", missing_modifier_t)

  hier_frame <- item$modifier$tbl %>% dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~paste0(item$geo, geo),
      geo = as.name(item$geo))), item$geo)) %>%
    dplyr::select_(.dots = c(modeled_params, item$modifier$modifiers)) %>%
    dplyr::rename_("param" = item$geo) %>%
    dplyr::mutate_("param" = ~paste0(item$geo, as.character(param))) %>%
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
    dplyr::summarise_each(~class(.)[1] %in% c("character", "factor", "ordered")) %>%
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
    dplyr::mutate_each_(~not_na, ~matches("_gt\\d+$"))
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

count_successes <- function(item, trial_counts, mean_group_outcome) {
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

format_counts <- function(item, trial_counts, success_counts) {
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

cast_missingness <- function(item, missingness) {
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
    nrow = length(item$filters$time),
    ncol = length(gt_names),
    dimnames = list(item$filters$time, gt_names))
  l2_only
}

make_dummy_l2_counts <- function(item) {
  array(0, c(item$T, item$Q, item$G_hier), list(item$filters$time,
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
    P = item$P,       # number of hierarchical parameters
    S = item$S,   # number of geographic units
    H = item$H,   # number of geographic-level predictors
    Hprior = item$H_prior,
    separate_t = item$control$separate_t,  # if 1, no pooling over time
    constant_item = item$control$constant_item,  # if 1, item parameters constant
    D = ifelse(item$control$constant_item, 1L, item$T),           # number of difficulty parameters
    WT = item$modifier$WT,            # weight matrix for calculating level-two mean
    l2_only = item$modifier$l2_only,
    Gl2 = item$G_hier,          # number of second-level groups
    delta_tbar_prior_mean = item$control$delta_tbar_prior_mean,
    delta_tbar_prior_sd = item$control$delta_tbar_prior_sd,
    innov_sd_delta_scale = item$control$innov_sd_delta_scale,
    innov_sd_theta_scale = item$control$innov_sd_theta_scale,
    group_counts = item$group_counts,
    vars = list(items = item$items,
      gt_items = grep("_gt\\d+$", colnames(item$tbl), value = TRUE),
      groups = item$groups,
      time_id = item$time,
      use_t = item$filters$time,
      geo_id = item$geo,
      periods = item$filters$time,
      survey_id = item$survey,
      covariate_groups = item$group_grid_t,
      hier_names = dimnames(item$modifier$group_design_matrix)[[2]]))
}
