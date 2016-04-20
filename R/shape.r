#' Shape data for a DGIRT model
#' @export
shape <- function(item_data,
                  ...,
                  modifier_data = NULL,
                  target_data = NULL,
                  aggregate_data = NULL) {

  control <- new("Control", ...)
  dgirt_in <- dgirtIn$new(control)

  check_targets(target_data, control)
  check_modifiers(modifier_data, control) 
  # TODO: check_aggregates(aggregate_data, control)
  check_item(item_data, control)

  item_data <- restrict_items(item_data, control)
  # FIXME: this is the only place where control is modified; preferable to avoid
  control@item_names <- intersect(control@item_names, names(item_data))
  modifier_data <- restrict_modifier(item_data, modifier_data, control)
  aggregate_data <- restrict_aggregates(aggregate_data, control)

  weight(item_data, target_data, control)
  dgirt_in$vars$gt_items <- discretize(item_data, control)

  dgirt_in$group_grid <- make_group_grid(item_data, aggregate_data, control)
  dgirt_in$group_grid_t <- make_group_grid_t(dgirt_in$group_grid, control)
  dgirt_in$group_counts <- make_group_counts(item_data,
                                              aggregate_data,
                                              dgirt_in,
                                              control)

  dgirt_in$n_vec <- setNames(dgirt_in$group_counts$n_grp, dgirt_in$group_counts$name)
  dgirt_in$s_vec <- setNames(dgirt_in$group_counts$s_grp, dgirt_in$group_counts$name)

  dgirt_in$MMM <- get_missing_groups(dgirt_in$group_counts,
                                      dgirt_in$group_grid,
                                      control)

  dgirt_in$G <- nrow(dgirt_in$group_grid_t)
  dgirt_in$G_hier <- ifelse(!length(modifier_data),
                          nlevels(gl(1L, dgirt_in$G)),
                          max(unlist(length(control@modifier_names)), 1L))
  dgirt_in$T <- length(control@time_filter)
  dgirt_in$Q <- length(c(intersect(control@item_names, names(item_data)),
                          intersect(control@aggregate_item_names, unique(aggregate_data$item))))

  dgirt_in$WT <- array(1, dim = c(dgirt_in$T, dgirt_in$G_hier, dgirt_in$G))

  dgirt_in$l2_only <- matrix(0L, nrow = length(control@time_filter), ncol = dgirt_in$Q)
  dgirt_in$NNl2 <- array(0L, dim = c(dgirt_in$T, dgirt_in$Q, dgirt_in$G_hier))
  dgirt_in$SSl2 <- dgirt_in$NNl2

  dgirt_in$XX <- make_design_matrix(item_data, dgirt_in, control)
  dgirt_in$ZZ <- shape_hierarchical_data(item_data, modifier_data, dgirt_in, control)
  dgirt_in$ZZ_prior <- dgirt_in$ZZ

  dgirt_in$D <- ifelse(control@constant_item, 1L, dgirt_in$T)
  dgirt_in$N <- nrow(dgirt_in$group_counts)
  dgirt_in$P <- ncol(dgirt_in$ZZ)
  dgirt_in$S <- dim(dgirt_in$ZZ)[[2]]
  dgirt_in$H <- dim(dgirt_in$ZZ)[[3]]
  dgirt_in$Hprior <- dgirt_in$H 

  dgirt_in$control <- control
  dgirt_in$item_data <- item_data
  dgirt_in$modifier_data <- modifier_data
  dgirt_in$aggregate_data <- aggregate_data
  dgirt_in$target_data <- target_data

  dgirt_in$vars = c(dgirt_in$vars,
                     list(items = control@item_names,
                          groups = control@group_names,
                          time_id = control@time_name,
                          use_t = control@time_filter,
                          geo_id = control@geo_name,
                          periods = control@time_filter,
                          survey_id = control@survey_name,
                          covariate_groups = dgirt_in$group_grid_t,
                          hier_names = dimnames(dgirt_in$ZZ)[[2]]))

  check(dgirt_in)
  dgirt_in
}

discretize <- function(item_data, control) {
  gt_table <- create_gt_variables(item_data, control)
  # NOTE: update item_data *by reference* to include gt_table
  item_data[, names(gt_table) := gt_table]
  # return the names in gt_table to safely reference its columns later without regex matching
  names(gt_table)
}

check <- function(dgirt_in) {
  check_dimensions(dgirt_in)
  check_values(dgirt_in)
  check_order(dgirt_in)
}

create_gt_variables <- function(item_data, control){
  widths <- c("item" = 30, "class" = 10, "levels" = 12, "responses" = 16)
  print_varinfo_header(widths)
  out <- lapply(control@item_names, function(i) {
    if (is.ordered(item_data[[i]])) {
      i_levels <- na.omit(levels(droplevels(item_data[[i]])))
      values <- match(as.character(item_data[[i]]), i_levels)
    } else if (is.numeric(item_data[[i]])) {
      i_levels <- sort.int(na.omit(unique.default(item_data[[i]])))
      values <- match(item_data[[i]], i_levels)
    } else {
      stop("each item should be an ordered factor or numeric")
    }
    gt_levels <- seq_along(i_levels)[-length(i_levels)]

    if (!length(gt_levels)) {
      stop("no variation in item ", deparse(i))
    } else if (identical(length(gt_levels), 1L)) {
      stopifnot(identical(length(i_levels), 2L))
    } else if (length(gt_levels) > 1L) {
      stopifnot(length(i_levels) > 2L)
    }

    print_varinfo(item_data, i, i_levels, gt_levels, widths)
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

print_varinfo <- function(item_data, i, i_levels, gt_levels, widths) {
  item_name = ifelse(nchar(i) > 30,paste0(stringi::stri_sub(i, 1, 26), '...'), i)
  responses = format(sum(!is.na(item_data[[i]])), big.mark = ",")
  values <- c("item" = item_name, "class" = class(item_data[[i]]), "levels" = length(i_levels), responses = responses)
  pad_side <- c("right", rep("left", length(values) - 1))
  assertthat::assert_that(identical(length(widths), length(values)))
  message(concat_varinfo(widths, values, pad_side))
}

concat_varinfo = function(widths, values, pad_side) {
  mapply(function(width, value, side) {
      paste0(stringi::stri_pad(value, width, side = side), collapse = '')
    }, widths, values, pad_side)
}

make_group_grid <- function(item_data, aggregate_data, control) {
  group_grid <- expand.grid(c(
    setNames(list(control@time_filter), control@time_name),
    lapply(rbind(item_data[, c(control@geo_name, control@group_names), with = FALSE],
                 aggregate_data[, c(control@group_names, control@geo_name), with = FALSE]),
           function(x) sort(unique(x)))),
    stringsAsFactors = FALSE)
  setDT(group_grid, key = c(control@time_name, control@group_names, control@geo_name))
  invisible(group_grid)
}

make_group_grid_t <- function(group_grid, control) {
  group_grid_t <- copy(group_grid)[, control@time_name := NULL, with = FALSE]
  group_grid_t <- group_grid_t[!duplicated(group_grid_t)]
  setkeyv(group_grid_t, c(control@group_names, control@geo_name))
  group_grid_t
}

restrict_items <- function(item_data, control) {
  item_data <- setDT(copy(item_data))
  extra_colnames <- setdiff(names(item_data),
                            c(control@item_names,
                              control@strata_names,
                              control@survey_name,
                              control@geo_name,
                              control@time_name,
                              control@group_names,
                              control@weight_name))
  if (length(extra_colnames)) {
    item_data[, c(extra_colnames) := NULL, with = FALSE]
  }
  coerce_factors(item_data, c(control@group_names, control@geo_name, control@survey_name))
  rename_numerics(item_data, control)
  initial_dim <- dim(item_data)
  final_dim <- c()
  iter <- 1L
  while (!identical(initial_dim, final_dim)) {
    message("Applying restrictions, pass ", iter, "...")
    if (identical(iter, 1L)) item_data <- drop_rows_missing_covariates(item_data, control)
    initial_dim <- dim(item_data)
    keep_t(item_data, control)
    keep_geo(item_data, control)
    drop_responseless_items(item_data, control)
    drop_items_rare_in_time(item_data, control)
    drop_items_rare_in_polls(item_data, control)
    final_dim <- dim(item_data)
    iter <- iter + 1L
    if (identical(initial_dim, final_dim)) {
      message("\tNo changes")
    } else {
      # FIXME: 
      message("\tRemaining: ", format(nrow(item_data), big.mark = ","), " rows, ",
              length(intersect(control@item_names, names(item_data))), " items")
    }
  }
  setkeyv(item_data, c(control@geo_name, control@time_name))
  invisible(item_data)
}

restrict_modifier <- function(item_data, modifier_data, control) {
  if (length(modifier_data)) {
    modifier_data <- setDT(copy(modifier_data))

    coerce_factors(modifier_data, c(control@modifier_names,
                                    control@t1_modifier_names,
                                    control@geo_name,
                                    control@time_name))

    modifier_data <- modifier_data[modifier_data[[control@geo_name]] %in% item_data[[control@geo_name]] &
                                   modifier_data[[control@time_name]] %in% item_data[[control@time_name]]]

    if (!identical(nrow(modifier_data),
                   nrow(unique(modifier_data[, c(control@geo_name, control@time_name), with = FALSE])))) {
      stop("time and geo identifiers don't uniquely identify modifier data observations")
    }
    message()
    message("Restricted modifier data to time and geo observed in item data.")
  }
  invisible(modifier_data)
}

restrict_aggregates <- function(aggregate_data, control) {
  if (length(aggregate_data)) {
    setDT(copy(aggregate_data))
    aggregate_data <- subset(aggregate_data,
                             aggregate_data[[control@geo_name]] %chin% control@geo_filter & 
                             aggregate_data[[control@time_name]] %in% control@time_filter)
    # subset to observed; FIXME: assumes n_grp variable name
    aggregate_data <- aggregate_data[n_grp > 0]
    extra_colnames <- setdiff(names(aggregate_data),
                              c(control@geo_name, control@time_name, control@group_names, "item", "s_grp", "n_grp"))
    if (length(extra_colnames)) {
      aggregate_data[, c(extra_colnames) := NULL, with = FALSE]
    }
    aggregate_data
  }
}

coerce_factors <- function(tbl, vars) {
  factor_vars <- vars[vapply(tbl[, vars, with = FALSE], is.factor, logical(1))]
  if (length(factor_vars)) {
    for (v in factor_vars) {
      tbl[, (v) := as.character(tbl[[v]])]
    }
  }
  invisible(tbl)
}

rename_numerics <- function(item_data, control) {
  varnames <- c(control@group_names, control@geo_name, control@modifier_names, control@t1_modifier_names)
  varnames <- varnames[vapply(item_data[, varnames], is.numeric, logical(1))]
  if (length(varnames)) {
    for (v in varnames) {
      item_data[, (v) := paste0(v, item_data[[v]])]
    }
  }
  invisible(item_data)
}

drop_rows_missing_covariates <- function(item_data, control) {
  n <- nrow(item_data)
  is_missing <- rowSums(is.na(item_data[, c(control@geo_name, control@time_name, control@group_names, control@survey_name), with = FALSE])) > 0
  item_data <- subset(item_data, !is_missing)
  if (!identical(n, nrow(item_data))) {
    message("\tDropped ", format(n - nrow(item_data), big.mark = ","), " rows for missingness in covariates")
  }
  item_data
}

with_contr.treatment <- function(...) {
  contrast_options = getOption("contrasts")
  options("contrasts"= c(unordered = "contr.treatment", ordered = "contr.treatment"))
  res <- eval(...)
  options("contrasts"= contrast_options)
  res
}

keep_t <- function(item_data, control) {
  data.table::setDT(item_data)
  item_data <- item_data[get(control@time_name) %in% control@time_filter]
  invisible(item_data)
}

keep_geo <- function(item_data, control) {
  data.table::setDT(item_data)
  item_data <- item_data[get(control@geo_name) %chin% control@geo_filter]
  invisible(item_data)
}

drop_responseless_items <- function(item_data, control) {
  item_names <- intersect(control@item_names, names(item_data))
  response_counts <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) == 0), .SDcols = item_names]
  responseless_items <- melt.data.table(response_counts, id.vars = NULL, measure.vars = names(response_counts))[(value)]
  responseless_items <- as.character(responseless_items[, variable])
  if (length(responseless_items)) {
    for (v in responseless_items) {
      item_data[, (v) := NULL]
    }
    message(sprintf(ngettext(length(responseless_items),
          "\tDropped %i item for lack of responses",
          "\tDropped %i items for lack of responses"),
        length(responseless_items)))
  }
  invisible(item_data)
}

drop_items_rare_in_time <- function(item_data, control) {
  item_names <- intersect(control@item_names, names(item_data))
  setkeyv(item_data, item_data[, control@time_name])
  response_t <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) > 0), .SDcols = item_names, by = eval(item_data[, control@time_name])]
  response_t <- melt.data.table(response_t, id.vars = control@time_name)[(value)]
  response_t <- response_t[, N := .N, by = variable]
  response_t <- response_t[N < control@min_t_filter]
  rare_items <- as.character(response_t[, variable])
  if (length(rare_items)) {
    for (v in rare_items) {
      item_data[, (v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_t requirement (%i)",
          "\tDropped %i items for failing min_t requirement (%i)"),
        length(rare_items), control@min_t_filter))
  }
  invisible(item_data)
}

drop_items_rare_in_polls <- function(item_data, control) {
  item_names <- intersect(control@item_names, names(item_data))
  #TODO: dedupe; cf. drop_items_rare_in_time
  setkeyv(item_data, item_data[, control@survey_name])
  item_survey <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) > 0), .SDcols = item_names, by = eval(item_data[, control@survey_name])]
  item_survey <- melt.data.table(item_survey, id.vars = control@survey_name)[(value)]
  item_survey <- item_survey[, N := .N, by = variable]
  item_survey <- item_survey[N < control@min_survey_filter]
  rare_items <- as.character(item_survey[, variable])
  if (length(rare_items)) {
    for (v in rare_items) {
      item_data[, (v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_survey requirement (%i)",
          "\tDropped %i items for failing min_survey requirement (%i)"),
        length(rare_items), control@min_survey_filter))
  }
  invisible(item_data)
}

make_group_counts <- function(item_data, aggregate_data, dgirt_in, control) {
  # item_data[, ("n_responses") := list(rowSums(!is.na(item_data[, dgirt_in$vars$gt_items, with = FALSE])))]
  item_data[, ("n_responses") := list(rowSums(!is.na(.SD))), .SDcols = dgirt_in$vars$gt_items]
  item_data[, ("def") := lapply(.SD, calc_design_effects), .SDcols = control@weight_name, with = FALSE,
           by = c(control@geo_name, control@group_names, control@time_name)]
  item_n_vars <- paste0(control@item_names, "_n_grp")
  item_mean_vars <- paste0(control@item_names, "_mean")

  item_n <- item_data[, lapply(.SD, function(x) ceiling(sum(as.integer(!is.na(x)) / get("n_responses") / get("def")))),
                       .SDcols = c(control@item_names), by = c(control@geo_name, control@group_names, control@time_name)]
  names(item_n) <- replace(names(item_n), match(control@item_names, names(item_n)), item_n_vars)
  setkeyv(item_n, c(control@time_name, control@geo_name, control@group_names))

  item_data[, ("adj_weight") := get(control@weight_name) / n_responses]
  item_means <- item_data[, lapply(.SD, function(x) weighted.mean(x, .SD$adj_weight)),
                       .SDcols = c(control@item_names, "adj_weight"), by = c(control@geo_name, control@group_names, control@time_name)]
  names(item_means) <- replace(names(item_means), match(control@item_names, names(item_means)), item_mean_vars)
  setkeyv(item_means, c(control@time_name, control@geo_name, control@group_names))

  counts_means <- item_n[item_means]
  counts_means <- counts_means[, c(control@time_name, control@geo_name, control@group_names, item_mean_vars, item_n_vars), with = FALSE]

  item_s_vars <- paste0(control@item_names, "_s_grp")
  counts_means[, (item_s_vars) := round(counts_means[, (item_mean_vars), with = FALSE] * counts_means[, (item_n_vars), with = FALSE], 0)]
  counts_means <- counts_means[, -grep("_mean$", names(counts_means)), with = FALSE]
  melted <- melt(counts_means, id.vars = c(control@time_name, control@geo_name, control@group_names), variable.name = "item")
  melted[, c("variable", "item") := list(gsub(".*([sn]_grp)$", "\\1", item), gsub("(.*)_[sn]_grp$", "\\1", item))]
  f <- as.formula(paste0(paste(control@time_name, control@geo_name, control@group_names, "item", sep = "+"), "~variable"))
  group_counts <- data.table::dcast.data.table(melted, f)
  # group trial and success counts cannot include NAs
  group_counts <- group_counts[n_grp != 0 & !is.na(n_grp)]
  group_counts[is.na(s_grp), s_grp := 0]

  # include aggregates, if any
  if (length(aggregate_data)) {
    message("Added ", length(control@aggregate_item_names), " items from aggregate data.")
    group_counts <- rbind(group_counts, aggregate_data)
  }
  group_counts
}

get_missing_groups <- function(group_counts, group_grid, control) {
  # group_counts <- dgirt_in$group_counts
  # group_grid <- dgirt_in$group_grid
  all_group_counts <- merge(group_counts, group_grid, all = TRUE,
                            by = c(control@group_names, control@geo_name, control@time_name))
  all_group_counts[, ("is_missing") := is.na(n_grp) + 0L]
  all_group_counts[is.na(n_grp), c("n_grp", "s_grp") := 1L]
  all_group_counts[, (control@geo_name) := paste0("x_", .SD[[control@geo_name]]), .SDcols = c(control@geo_name)]
  acast_formula <- as.formula(paste0(control@time_name, "~ item ~", paste(control@group_names, collapse = "+"), "+", control@geo_name))
  MMM <- reshape2::acast(all_group_counts, acast_formula, value.var = "is_missing", fill = 1)
  # merging group_grid included unobserved combinations of grouping variables; being unobserved, they're associated with
  # no item, and when the result is cast to array, NA will appear in the second dimension as an item name
  MMM <- MMM[, dimnames(MMM)[[2]] != "NA", , drop = FALSE]
  stopifnot(all(MMM %in% c(0, 1)))
  MMM
}

shape_hierarchical_data <- function(item_data, modifier_data, dgirt_in, control) {
  if (dgirt_in$G_hier == 1) {
    zz.names <- list(control@time_filter, dimnames(dgirt_in$XX)[[2]], "")
    zz <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  } else {
    # the array of hierarchical data ZZ should be T x P x H, where T is the number of time periods, P is the number of
    # hierarchical parameters (including the geographic), and H is the number of predictors for geographic unit effects
    # TODO: make flexible; as written we must model geo x t
    modeled_params = c(control@geo_name, control@time_name)
    unmodeled_params = setdiff(c(control@geo_name, control@time_name, control@group_names), modeled_params)
    # TODO: uniqueness checks on level2 data (state_demographics should not pass)
    # TODO: confirm sort order of level2_modifier

    # does hierarchical data include all observed geo?
    missing_modifier_geo <- setdiff(
                                    unique(dgirt_in$group_grid_t[[control@geo_name]]),
                                    unique(modifier_data[[control@geo_name]]))
    if (length(missing_modifier_geo)) stop("no hierarchical data for geo in item data: ", missing_modifier_geo)
    # does hierarchical data include all modeled t?
    missing_modifier_t <- setdiff(control@time_filter, unique(modifier_data[[control@time_name]]))
    if (length(missing_modifier_t)) stop("missing hierarchical data for t in item data: ", missing_modifier_t)

    # NOTE: we're prepending the value of geo with its variable name; may not be desirable
    hier_frame <- copy(modifier_data)[, control@geo_name := paste0(control@geo_name, modifier_data[[control@geo_name]])]
    hier_frame[, setdiff(names(hier_frame), c(modeled_params, modifier_names)) := NULL, with = FALSE]
    hier_frame[, c("param", control@geo_name) := .(hier_frame[[control@geo_name]], NULL), with = FALSE]
    setkeyv(hier_frame, c("param", control@time_name))

    modeled_param_names <- unique(hier_frame[, param])
    unmodeled_param_levels = unlist(lapply(unmodeled_params, function(x) {
                                             paste0(x, unique(dgirt_in$group_grid_t[[x]]))[-1]
        }))
    param_levels <- c(modeled_param_names, unmodeled_param_levels)

    unmodeled_frame <- expand.grid(c(list(
                                          unmodeled_param_levels,
                                          sort(unique(hier_frame[[control@time_name]])),
                                          as.integer(rep(list(0), length(modifier_names))))))
    unmodeled_frame <- setNames(unmodeled_frame, c("param", control@time_name, modifier_names))
    setDT(unmodeled_frame, key = c("param", control@time_name))

    hier_frame <- rbind(hier_frame, unmodeled_frame)

    # FIXME: hacky handling of the possibility of NA modifier values here
    hier_frame[, c(modifier_names) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = modifier_names]

    hier_melt = melt(hier_frame, id.vars = c("param", control@time_name), variable.name = "modifiers", variable.factor = FALSE,
                     value.factor = FALSE)
    setkeyv(hier_melt, c("param", "year"))

    melt_formula <- as.formula(paste(control@time_name, "param", "modifiers", sep = " ~ "))
    zz <- reshape2::acast(hier_melt, melt_formula, drop = FALSE, value.var = "value")
    zz <- zz[, -1, , drop = FALSE]
  } 
  zz
}

make_design_matrix <- function(item_data, dgirt_in, control) {
  design_formula <- as.formula(paste("~ 0", control@geo_name, paste(control@group_names, collapse = " + "), sep = " + "))
  design_matrix <- with_contr.treatment(model.matrix(design_formula, dgirt_in$group_grid_t))
  rownames(design_matrix) <- paste(paste(dgirt_in$group_grid_t[[control@group_names]], sep = "_"),
                                   dgirt_in$group_grid_t[[control@geo_name]],  sep = "_x_")
  design_matrix <- subset(design_matrix, select = -1)
  # TODO: move to S4 validate
  invalid_values <- setdiff(as.vector(design_matrix), c(0, 1))
  if (length(invalid_values)) {
    stop("design matrix values should be in (0, 1); found ", paste(sort(invalid_values), collapse = ", "))
  }
  design_matrix
}

get_gt <- function(item_data) {
  copy(item_data)[, grep("_gt\\d+$", names(item_data), value = TRUE), with = FALSE]
}

calc_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}

concat_groups <- function(tabular, control, geo_id, name) {
  has_all_names(tabular, control@group_names)
  has_all_names(tabular, geo_id)
  # FIXME: importing tidyr for this alone isn't necessary
  tabular %>%
    tidyr::unite_("group_concat", control@group_names, sep = "_") %>%
    tidyr::unite_(name, c("group_concat", geo_id), sep = "_x_")
}

split_groups <- function(tabular, control, geo_id, name) {
  assertthat::assert_that(has_name(tabular, "name"))
  tabular %>%
    tidyr::separate_(name, c("group_concat", geo_id), sep = "_x_") %>%
    tidyr::separate_("group_concat", control@group_names, sep = "_")
}

set_G <- function(item_data, control) {
  # TODO: should this be set from e.g. group_grid?
  group_levels <- sapply(item_data[, c(control@geo_name, control@group_names), with = FALSE],
                         function(x) length(unique(x)))
  Reduce(`*`, group_levels)
}
