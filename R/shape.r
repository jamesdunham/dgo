#' Shape item response data for modeling with dgirt
#' @export
# data(state_opinion)
# data = list(level1 = state_opinion,
#        level2 = dplyr::mutate(state_opinion, education = sample(1:2, nrow(state_opinion), replace = TRUE)) %>%
#          dplyr::distinct(state, year))
# vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
#             groups = c("race"),
#             time_id = "year",
#             geo_id = "state",
#             survey_id = "source",
#             survey_weight = "weight",
#             level2_modifiers = "education",
#             level2_period1_modifiers = "education")
# filters = list(periods = c(2006:2010))
# item <- wrangle_to_shape()

# data = list(level1 = state_opinion)
# vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
#             groups = c("race"),
#             time_id = "year",
#             geo_id = "state",
#             survey_id = "source",
#             survey_weight = "weight")
# item <- wrangle_to_shape()

shape <- function(item, control = list(...)) {
  setDT(item$tbl, key = c(item$time, item$geo, item$groups))
  if (item$has_hierarchy()) {
    setDT(item$modifier$tbl, key = c(item$modifier$geo, item$modifier$time))
  }
  restrict(item)
  reweight(item)
  dichotomize(item)
  add_counts(item)
  get_missing_groups(item)
  add_hierarchical(item)
  stan_data <- tostan(item)
  check(stan_data)
  stan_data
}

restrict <- function(item) {
  restrict_items(item)
  restrict_modifier(item)
}

reweight <- function(item) {
  # TODO: this is unclear; instead if $has_targets() or somethign
  if (inherits(item$targets$tbl, "data.frame")) {
    weight(item)
  }
}

dichotomize <- function(item) {
  # FIXME: NA values in item variables -> 0 in dichotomized variables  
  gt_table <- create_gt_variables(item)
  item$tbl[, names(gt_table) := gt_table]
  invisible(item)
}

add_counts <- function(item) {
  make_group_grid(item)
  make_group_grid_t(item)
  make_group_counts(item)
}

add_hierarchical <- function(item) {
  item$modifier$WT <- array(1, dim = c(item$T, item$G_hier, item$G))
  item$modifier$l2_only <- make_dummy_l2_only(item)
  item$modifier$NNl2 <- make_dummy_l2_counts(item)
  item$modifier$SSl2 <- make_dummy_l2_counts(item)
  item$modifier$group_design_matrix <- make_design_matrix(item)
  item$modifier$ZZ <- make_hierarchical_array(item)
  item$modifier$ZZ_prior <- make_hierarchical_array(item)
}

check <- function(stan_data) {
  check_dimensions(stan_data)
  check_values(stan_data)
  check_order(stan_data)
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

    if (length(gt_levels) < 1) {
      stop("no variation in item ", deparse(i))
    } else if (identical(length(gt_levels), 1L)) {
      assertthat::assert_that(has_length(i_levels, 2))
    } else if (length(gt_levels) > 1L) {
      assertthat::assert_that(length(i_levels) > 2L)
    }

    print_varinfo(item$tbl, i, item_levels = i_levels, gt_levels = gt_levels, widths)
    gt_cols <- lapply(gt_levels, function(gt) {
      is_greater(values, gt)
    })
    assertthat::assert_that(not_empty(gt_cols))
    setNames(gt_cols, paste(i, gt_levels, sep = "_gt"))
  })
  print_varinfo_rule(widths)
  data.frame(out)
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
    lapply(item$tbl[, c(item$geo, item$groups), with = FALSE], function(x) sort(unique(x)))),
    stringsAsFactors = FALSE)
  item$group_grid <- setDT(group_grid, key = c(item$time, item$groups, item$geo))
  invisible(item)
}

make_group_grid_t <- function(item) {
  group_grid_t <- copy(item$group_grid)[, item$time := NULL, with = FALSE]
  group_grid_t <- group_grid_t[!duplicated(group_grid_t)]
  item$group_grid_t <- setkeyv(group_grid_t, c(item$groups, item$geo))
  item$check_groups(group_grid_t)
  invisible(item)
}

restrict_items <- function(item) {
  coerce_factors(item$tbl, c(item$groups, item$geo, item$survey))
  rename_numerics(item)
  initial_dim <- dim(item$tbl)
  final_dim <- c()
  iter <- 1L
  while (!identical(initial_dim, final_dim)) {
    message("Applying restrictions, pass ", iter, "...")
    if (identical(iter, 1L)) drop_rows_missing_covariates(item)
    initial_dim <- dim(item$tbl)
    keep_t(item)
    drop_responseless_items(item)
    drop_items_rare_in_time(item)
    drop_items_rare_in_polls(item)
    not_empty(item$tbl)
    final_dim <- dim(item$tbl)
    iter <- iter + 1L
    if (identical(initial_dim, final_dim)) {
      message("\tNo changes")
    } else {
      message("\tRemaining: ", format(nrow(item$tbl), big.mark = ","), " rows, ", length(item$items), " items")
    }
  }
  setkeyv(item$tbl, c(item$modifier$geo, item$modifier$time))
  invisible(item)
}

restrict_modifier <- function(item) {
  if (item$has_hierarchy()) {

    coerce_factors(item$modifier$tbl, c(item$modifier$modifiers, item$modifier$t1_modifiers, item$modifier$geo,
                                        item$modifier$time))

    initial_nrow <- nrow(item$modifier$tbl)
    initial_dim <- dim(item$tbl)

    item$modifier$tbl <- item$modifier$tbl[item$modifier$tbl[[item$modifier$geo]] %in% item$tbl[[item$geo]] &
                                           item$modifier$tbl[[item$modifier$time]] %in% item$tbl[[item$time]]]

    if (!identical(nrow(item$modifier$tbl),
                   nrow(unique(item$modifier$tbl[, c(item$modifier$geo, item$modifier$time), with = FALSE])))) {
      stop("time and geo identifiers don't uniquely identify modifier data observations")
    }
    message()
    message("Restricted modifier data to time and geo observed in item data.")
  }
  invisible(item)
}

coerce_factors <- function(tbl, vars) {
  factor_vars <- vars[vapply(tbl[, vars, with = FALSE], is.factor, logical(1))]
  for (v in factor_vars) {
    tbl[, (v) := as.character(tbl[[v]])]
  }
  invisible(tbl)
}

rename_numerics <- function(item) {
  varnames <- intersect(names(item$tbl), c(item$groups, item$geo, item$modifier$modifiers, item$modifier$t1_modifiers, item$modifier$geo))
  varnames <- varnames[vapply(item$tbl[, varnames], is.numeric, logical(1))]
  for (v in varnames) {
    item$tbl[, (v) := paste0(v, item$tbl[[v]])]
  }
}

drop_rows_missing_covariates <- function(item) {
  n <- nrow(item$tbl)
  data.table::setDT(item$tbl)
  is_missing <- rowSums(is.na(item$tbl[, c(item$geo, item$time, item$groups, item$survey), with = FALSE])) > 0
  item$tbl <- subset(item$tbl, !is_missing)
  sum(is.na(item$tbl$race))
  if (!identical(n, nrow(item$tbl))) {
    message("\tDropped ", format(n - nrow(item$tbl), big.mark = ","), " rows for missingness in covariates")
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
  data.table::setDT(item$tbl)
  item$tbl <- item$tbl[get(item$time) %in% item$filters$time]
  item
}

drop_itemless_respondents <- function(item) {
  item$tbl <- data.table::as.data.table(item$tbl)
  n <- nrow(item$tbl)
  item$items <- intersect(item$items, colnames(item$tbl))
  item$tbl[, rowselect := (!Reduce("*", item$tbl[, lapply(.SD, is.na), .SDcols = item$items]))]
  setkey(item$tbl, rowselect)
  item$tbl[TRUE][, rowselect := NULL]
  if (!identical(n, nrow(item$tbl))) {
    message(sprintf(ngettext(nrow(item$tbl) - n,
          "\tDropped  %s row for lack of item responses",
          "\tDropped %s rows for lack of item responses"),
        format(nrow(item$tbl) - n, big.mark = ",")))
  }
  item
}

drop_responseless_items <- function(item) {
  setDT(item$tbl)
  response_counts <- item$tbl[, lapply(.SD, function(x) sum(!is.na(x)) == 0), .SDcols = item$items]
  responseless_items <- melt.data.table(response_counts, id.vars = NULL, measure.vars = names(response_counts))[(value)]
  responseless_items <- as.character(responseless_items[, variable])
  item$items <- setdiff(item$items, responseless_items)
  if (length(responseless_items) > 0) {
    for (v in responseless_items) {
      item$tbl[, (v) := NULL]
    }
    message(sprintf(ngettext(length(responseless_items),
          "\tDropped %i item for lack of responses",
          "\tDropped %i items for lack of responses"),
        length(responseless_items)))
  }
  item
}

drop_items_rare_in_time <- function(item) {
  setDT(item$tbl)
  setkeyv(item$tbl, item$tbl[, item$time])
  response_t <- item$tbl[, lapply(.SD, function(x) sum(!is.na(x)) > 0), .SDcols = item$items, by = eval(item$tbl[, item$time])]
  response_t <- melt.data.table(response_t, id.vars = item$time)[(value)]
  response_t <- response_t[, N := .N, by = variable]
  response_t <- response_t[N < item$filters$min_t]
  rare_items <- as.character(response_t[, variable])
  item$items <- setdiff(item$items, rare_items)
  if (length(rare_items) > 0) {
    for (v in rare_items) {
      item$tbl[, (v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_t requirement (%i)",
          "\tDropped %i items for failing min_t requirement (%i)"),
        length(rare_items), item$filters$min_t))
  }
  item
}

drop_items_rare_in_polls <- function(item) {
  #TODO: dedupe; cf. drop_items_rare_in_time
  setDT(item$tbl)
  setkeyv(item$tbl, item$tbl[, item$survey])
  item_survey <- item$tbl[, lapply(.SD, function(x) sum(!is.na(x)) > 0), .SDcols = item$items, by = eval(item$tbl[, item$survey])]
  item_survey <- melt.data.table(item_survey, id.vars = item$survey)[(value)]
  item_survey <- item_survey[, N := .N, by = variable]
  item_survey <- item_survey[N < item$filters$min_survey]
  rare_items <- as.character(item_survey[, variable])
  item$items <- setdiff(item$items, rare_items)
  if (length(rare_items) > 0) {
    for (v in rare_items) {
      item$tbl[, (v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_survey requirement (%i)",
          "\tDropped %i items for failing min_survey requirement (%i)"),
        length(rare_items), item$filters$min_survey))
  }
  item
}

make_group_counts <- function(item) {
  item$tbl[, ("n_responses") := list(rowSums(!is.na(get_gt(item))))]
  item$tbl[, ("def") := lapply(.SD, calc_design_effects), .SDcols = item$weight, with = FALSE,
           by = c(item$geo, item$groups, item$time)]
  item_vars <- grep("_gt\\d+$", names(item$tbl), value = TRUE)
  item_n_vars <- paste0(item_vars, "_n_grp")
  item_mean_vars <- paste0(item_vars, "_mean")

  item_n <- item$tbl[, lapply(.SD, function(x) ceiling(sum(as.integer(!is.na(x)) / get("n_responses") / get("def")))),
                       .SDcols = c(item_vars), by = c(item$geo, item$groups, item$time)]
  names(item_n) <- replace(names(item_n), match(item_vars, names(item_n)), item_n_vars)
  setkeyv(item_n, c(item$time, item$geo, item$groups))

  item$tbl[, ("adj_weight") := get(item$weight) / n_responses]
  item_means <- item$tbl[, lapply(.SD, function(x) weighted.mean(x, .SD$adj_weight)),
                       .SDcols = c(item_vars, "adj_weight"), by = c(item$geo, item$groups, item$time)]
  names(item_means) <- replace(names(item_means), match(item_vars, names(item_means)), item_mean_vars)
  setkeyv(item_means, c(item$time, item$geo, item$groups))

  counts_means <- item_n[item_means]
  counts_means <- counts_means[, c(item$time, item$geo, item$groups, item_mean_vars, item_n_vars), with = FALSE]

  item_s_vars <- paste0(item_vars, "_s_grp")
  counts_means[, (item_s_vars) := round(counts_means[, (item_mean_vars), with = FALSE] * counts_means[, (item_n_vars), with = FALSE], 0)]
  counts_means <- counts_means[, -grep("_mean$", names(counts_means)), with = FALSE]
  melted <- melt(counts_means, id.vars = c(item$time, item$geo, item$groups), variable.name = "item")
  melted[, c("variable", "item") := list(gsub(".*([sn]_grp)$", "\\1", item), gsub("(.*)_[sn]_grp$", "\\1", item))]
  f <- as.formula(paste0(paste(item$time, item$geo, item$groups, "item", sep = "+"), "~variable"))
  item$group_counts <- data.table::dcast.data.table(melted, f)
  # group trial and success counts cannot include NAs
  item$group_counts <- item$group_counts[n_grp != 0 & !is.na(n_grp)]
  item$group_counts[is.na(s_grp), s_grp := 0]
  invisible(item)
}

get_missing_groups <- function(item) {
  all_group_counts <- merge(item$group_counts, item$group_grid, all = TRUE)
  all_group_counts[, ("is_missing") := is.na(n_grp) + 0L]
  all_group_counts[is.na(n_grp), c("n_grp", "s_grp") := 1L]
  all_group_counts[, (item$geo) := paste0("x_", .SD[[item$geo]]), .SDcols = c(item$geo)]
  acast_formula <- as.formula(paste0(item$time, "~ item ~", paste(item$groups, collapse = "+"), "+", item$geo))
  MMM <- reshape2::acast(all_group_counts, acast_formula, value.var = "is_missing", fill = 1)
  # merging group_grid included unobserved combinations of grouping variables; being unobserved, they're associated with
  # no item, and when the result is cast to array, NA will appear in the second dimension as an item name
  MMM <- MMM[, dimnames(MMM)[[2]] != "NA", ]
  table(MMM, useNA = 'always')
  assertthat::assert_that(all_in(MMM, c(0, 1)))
  item$MMM <- MMM
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
  
  # does hierarchical data include all observed geo?
  missing_modifier_geo <- setdiff(
    unique(item$group_grid_t[[item$modifier$geo]]),
    unique(item$modifier$tbl[[item$modifier$geo]]))
  if (length(missing_modifier_geo) > 0) stop("no hierarchical data for geo in item data: ", missing_modifier_geo)
  # does hierarchical data include all modeled t?
  missing_modifier_t <- setdiff(item$filters$time, unique(item$modifier$tbl[[item$modifier$time]]))
  if (length(missing_modifier_t) > 0) stop("missing hierarchical data for t in item data: ", missing_modifier_t)

  # NOTE: we're prepending the value of geo with its variable name; may not be desirable
  hier_frame <- copy(item$modifier$tbl)[, item$geo := paste0(item$geo, item$modifier$tbl[[item$geo]])]
  hier_frame[, setdiff(names(hier_frame), c(modeled_params, item$modifier$modifiers)) := NULL, with = FALSE]
  hier_frame[, c("param", item$modifier$geo) := .(hier_frame[[item$modifier$geo]], NULL), with = FALSE]
  setkeyv(hier_frame, c("param", item$modifier$time))

  modeled_param_names <- unique(hier_frame[, param])
  unmodeled_param_levels = unlist(lapply(unmodeled_params, function(x) {
      paste0(x, unique(item$group_grid_t[[x]]))[-1]
    }))
  param_levels <- c(modeled_param_names, unmodeled_param_levels)

  unmodeled_frame <- expand.grid(c(list(
      unmodeled_param_levels,
      sort(unique(hier_frame[[item$modifier$time]])),
      as.integer(rep(list(0), length(item$modifier$modifiers))))))
  unmodeled_frame <- setNames(unmodeled_frame, c("param", item$modifier$time, item$modifier$modifiers))
  setDT(unmodeled_frame, key = c("param", item$modifier$time))

  hier_frame <- rbind(hier_frame, unmodeled_frame)

  # FIXME: hacky handling of the possibility of NA modifier values here
  hier_frame[, c(item$modifier$modifiers) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = item$modifier$modifiers]

  hier_melt = melt(hier_frame, id.vars = c("param", item$time), variable.name = "modifiers", variable.factor = FALSE,
                   value.factor = FALSE)
  setkeyv(hier_melt, c("param", "year"))

  melt_formula <- as.formula(paste(item$time, "param", "modifiers", sep = " ~ "))
  zz <- reshape2::acast(hier_melt, melt_formula, drop = FALSE, value.var = "value")
  zz <- zz[, -1, , drop = FALSE]
  zz
}

make_design_matrix <- function(item) {
  design_formula <- as.formula(paste("~ 0", item$geo, paste(item$groups, collapse = " + "), sep = " + "))
  design_matrix <- with_contr.treatment(model.matrix(design_formula, item$group_grid_t))
  rownames(design_matrix) <- paste(paste(item$group_grid_t[[item$groups]], sep = "_"),
                                  item$group_grid_t[[item$geo]],  sep = "_x_")
  design_matrix <- subset(design_matrix, select = -1)
  invalid_values <- setdiff(as.vector(design_matrix), c(0, 1))
  if (length(invalid_values) > 0) {
    stop("design matrix values should be in (0, 1); found ", paste(sort(invalid_values), collapse = ", "))
  }
  return(design_matrix)
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

get_gt <- function(item) {
  copy(item$tbl)[, grep("_gt\\d+$", names(item$tbl), value = TRUE), with = FALSE]
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
  list(n_vec = setNames(item$group_counts$n_grp, item$group_counts$name),
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
       D = ifelse(item$control$constant_item, 1L, T),           # number of difficulty parameters
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
