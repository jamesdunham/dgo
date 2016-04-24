#' Shape Model Data.
#'
#' `shape` prepares data for modeling with `dgirt`.
#'
#' `shape` takes four kinds of data. Only `item_data` is required. For each data
#' passed to `shape` through its `*_data` arguments, additional arguments are
#' required. These are described in the sections below. Most give the name or
#' names of key variables in the data; they end in `_name` or `_names` and
#' should be character vectors. A few implement preprocessing and modeling
#' choices.
#' 
#' @section Item Data:
#' These are required arguments if `item_data` is given.
#' \describe{
#'   \item{item_names}{Individual item responses. These variables should be
#'   integers or ordered factors in the data.}
#'   \item{group_names}{Discrete grouping variables, usually demographic. Using
#'   numeric variables is allowed but not recommended.}
#'   \item{geo_name}{A geographic variable representing local areas. In the
#'   data it should should be character or factor.}
#'   \item{time_name}{A time variable with numeric values.}
#'   \item{weight_name}{A variable giving survey weights.}
#' }
#'
#' @section Modifier Data:
#' These are required arguments if `modifier_data` is given.
#' \describe{
#'   \item{modifier_names}{Modifiers of geographic hierarchical parameters, e.g.
#'   median household income in each local-area and time-period combination.}
#'   \item{t1_modifier_names}{Modifiers to be used instead of those in
#'   `modifier_names` only in the first period.}
#' }
#'
#' @section Aggregate Data:
#' Specifying `aggregate_data` requires no additional arguments; instead, we
#' make many assumptions about the data. This implementation is likely to change
#' in the future. Variable names given for `item_data` are expected in the table
#' of aggregates: `item_names`, `group_names`, `geo_name`, and `time_name`. Two
#' fixed variable names are also expected in `aggregate_data`: `n_grp` giving
#' adjusted counts of item-response trials, and `s_grp` giving adjusted counts
#' of item-response successes. The counts should be adjusted consistently with
#' the transformations applied to the individual `item_data`.
#'
#' @section Preprocessing:
#' If `target_data` is specified `shape` will adjust the weighting of groups
#' toward population targets via raking. This relies on an adaptation of
#' `\link[survey]{rake}`. The additional required arguments are
#' `target_proportion_name` and `strata_names`. The implementation will be more
#' flexible in the future, but at the moment the strata are defined additively
#' when more than one variable is given in `strata_names`. 
#' 
#' `shape` can restrict data row-wise in `item_data`, `modifier_data`, and
#' `aggregate_data` to that within specified time periods (`time_filter`) and
#' local geographic areas (`geo_filter`). Data can also be filtered for
#' sparsity, to keep items that appear in a minimum of time periods or surveys.
#' This is a column-wise operation. If both row-wise and column-wise
#' restrictions are specified, `shape` iterates over them until they leave the
#' data unchanged.
#'
#' \describe{
#'   \item{target_proportion_name}{The variable giving population proportions
#'   for strata.}
#'   \item{strata_names}{Variables that define population strata.}
#'   \item{geo_filter}{A character vector giving values of the geographic
#'   variable. Defaults to observed values.}
#'   \item{time_filter}{A numeric vector giving possible values of the time
#'   variable. Observed and unobserved time periods can be given.}
#'   \item{min_survey_filter}{An integer minimum of survey appearances for
#'   included items. Defaults to 1 and requires `survey_name`.}
#'   \item{survey_name}{The variable that identifies surveys, for use with
#'   `min_survey_filter`.}
#'   \item{min_t_filter}{An integer minimum of time period appearances for
#'   included items.}
#' }
#'
#' @section Modeling Choices:
#' Each of these arguments is optional. They may move to the `dgirt` signature
#' in the future.
#' \describe{
#'   \item{constant_item}{Whether item difficulty parameters should be constant
#'   over time. Default `TRUE`.}
# FIXME: Removed but not yet moved to dgirt:
#'   \item{separate_t}{Whether smoothing of estimates over time should be
#'   disabled. Default `FALSE`.}
#'   \item{delta_tbar_prior_mean}{Prior mean for `delta_tbar`, the normal weight
#'   on `theta_bar` in the previous period.  Default `0.5`.}
#'   \item{delta_tbar_prior_sd}{Prior standard deviation for `delta_bar`.
#'   Default `0.5`.}
#'   \item{innov_sd_delta_scale}{Prior scale for `sd_innov_delta`, the Cauchy
#'   innovation standard deviation of `nu_geo` and `delta_gamma`. Default `2.5`.}
#'   \item{innov_sd_theta_scale}{Prior scale for `sd_innov_theta`, the Cauchy
#'   innovation standard deviation of `gamma`, `xi`, and if `constant_item` is
#'   `FALSE` the item difficulty `diff`. Default `2.5`.}
#' }
#' @param item_data A table of individual item responses.
#' @param ... Further arguments. See details below.
#' @param modifier_data A table giving characteristics of `item_data`
#' identifiers to be modeled as hierarchical parameters.
#' @param target_data A table of population proportions.
#' @param aggregate_data A table of group-level item-response aggregates.
#' @return An object of the type expected by `\link{dgirt}`.
#' @import data.table
#' @examples
#' # model individual item responses 
#' data(state_opinion)
#' shaped_responses <- shape(state_opinion,
#'                           time_name = "year",
#'                           geo_name = "state",
#'                           item_names = "Q_cces2006_minimumwage",
#'                           group_names = "race",
#'                           survey_name = "source",
#'                           weight_name = "weight")
#' @export
shape <- function(item_data,
                  ...,
                  modifier_data = NULL,
                  target_data = NULL,
                  aggregate_data = NULL) {

  ctrl <- init_control(item_data, modifier_data, target_data, aggregate_data,
                       ...)

  d_in <- dgirtIn$new(item_data, modifier_data, target_data, aggregate_data,
                      ctrl)

  check_targets(target_data, ctrl)
  check_modifiers(modifier_data, ctrl) 
  # TODO: check_aggregates(aggregate_data, ctrl)
  check_item(item_data, ctrl)

  item_data <- restrict_items(item_data, ctrl)
  # FIXME: preferable to avoid modifying ctrl
  ctrl@item_names <- intersect(ctrl@item_names, names(item_data))
  modifier_data <- restrict_modifier(item_data, modifier_data, ctrl)
  aggregate_data <- restrict_aggregates(aggregate_data, ctrl)
  ctrl@aggregate_item_names <-
    ctrl@aggregate_item_names[ctrl@aggregate_item_names %chin%
                              aggregate_data$item]

  weight(item_data, target_data, ctrl)
  d_in$gt_items <- discretize(item_data, ctrl)

  d_in$group_grid <- make_group_grid(item_data, aggregate_data, ctrl)
  d_in$group_grid_t <- make_group_grid_t(d_in$group_grid, ctrl)
  d_in$group_counts <- make_group_counts(item_data, aggregate_data, d_in, ctrl)

  d_in$n_vec <- setNames(d_in$group_counts$n_grp, d_in$group_counts$name)
  d_in$s_vec <- setNames(d_in$group_counts$s_grp, d_in$group_counts$name)

  d_in$MMM <- get_missing_groups(d_in$group_counts, d_in$group_grid, ctrl)

  d_in$G <- nrow(d_in$group_grid_t)
  d_in$G_hier <- ifelse(!length(modifier_data), nlevels(gl(1L, d_in$G)),
                        max(unlist(length(ctrl@modifier_names)), 1L))
  d_in$T <- length(ctrl@time_filter)
  d_in$Q <- length(c(intersect(ctrl@item_names, names(item_data)),
                     intersect(ctrl@aggregate_item_names,
                               unique(aggregate_data$item))))

  d_in$WT <- array(1, dim = c(d_in$T, d_in$G_hier, d_in$G))

  d_in$l2_only <- matrix(0L, nrow = length(ctrl@time_filter), ncol = d_in$Q)
  d_in$NNl2 <- array(0L, dim = c(d_in$T, d_in$Q, d_in$G_hier))
  d_in$SSl2 <- d_in$NNl2

  d_in$XX <- make_design_matrix(item_data, d_in, ctrl)
  d_in$ZZ <- shape_hierarchical_data(item_data, modifier_data, d_in, ctrl)
  d_in$ZZ_prior <- d_in$ZZ
  d_in$hier_names <- dimnames(d_in$ZZ)[[2]]

  d_in$D <- ifelse(ctrl@constant_item, 1L, d_in$T)
  d_in$N <- nrow(d_in$group_counts)
  d_in$P <- ncol(d_in$ZZ)
  d_in$S <- dim(d_in$ZZ)[[2]]
  d_in$H <- dim(d_in$ZZ)[[3]]
  d_in$Hprior <- d_in$H 

  d_in$control <- ctrl
  d_in$item_data <- item_data
  d_in$modifier_data <- modifier_data
  d_in$aggregate_data <- aggregate_data
  d_in$target_data <- target_data
  d_in$call <- match.call()

  check(d_in)
  d_in
}

discretize <- function(item_data, ctrl) {
  # Discretize item response variables
  #
  # For item response variables with K ordered levels, make K - 1 indicators for
  # whether a response is ranked higher than k.
  gt_table <- create_gt_variables(item_data, ctrl)
  # NOTE: updating item_data by reference to include gt_table
  item_data[, names(gt_table) := gt_table]
  # return the indicator names to reference them safely later
  names(gt_table)
}

check <- function(d_in) {
  check_dimensions(d_in)
  check_values(d_in)
  check_order(d_in)
}

create_gt_variables <- function(item_data, ctrl){
  widths <- c("item" = 30, "class" = 10, "levels" = 12, "responses" = 16)
  print_varinfo_header(widths)
  out <- lapply(ctrl@item_names, function(i) {
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

make_group_grid <- function(item_data, aggregate_data, ctrl) {
  # Make a table giving combinations of the grouping variables
  group_grid <- expand.grid(c(
    setNames(list(ctrl@time_filter), ctrl@time_name),
    lapply(rbind(item_data[, c(ctrl@geo_name, ctrl@group_names), with = FALSE],
                 aggregate_data[, c(ctrl@group_names, ctrl@geo_name), with =
                                FALSE]),
           function(x) sort(unique(x)))), stringsAsFactors = FALSE)
  setDT(group_grid, key = c(ctrl@time_name, ctrl@group_names, ctrl@geo_name))
  invisible(group_grid)
}

make_group_grid_t <- function(group_grid, ctrl) {
  # Make a table giving combinations of grouping variables, excluding time
  group_grid_t <- copy(group_grid)[, ctrl@time_name := NULL, with = FALSE]
  group_grid_t <- group_grid_t[!duplicated(group_grid_t)]
  setkeyv(group_grid_t, c(ctrl@group_names, ctrl@geo_name))
  group_grid_t
}

restrict_items <- function(item_data, ctrl) {
  setDT(item_data)
  extra_colnames <- setdiff(names(item_data),
                            c(ctrl@item_names,
                              ctrl@strata_names,
                              ctrl@survey_name,
                              ctrl@geo_name,
                              ctrl@time_name,
                              ctrl@group_names,
                              ctrl@weight_name))
  if (length(extra_colnames)) {
    item_data[, c(extra_colnames) := NULL]
  }
  coerce_factors(item_data, c(ctrl@group_names, ctrl@geo_name,
                              ctrl@survey_name))
  rename_numerics(item_data, ctrl)
  initial_dim <- dim(item_data)
  final_dim <- c()
  iter <- 1L
  while (!identical(initial_dim, final_dim)) {
    message("Applying restrictions, pass ", iter, "...")
    if (identical(iter, 1L)) item_data <-
      drop_rows_missing_covariates(item_data, ctrl)
    initial_dim <- dim(item_data)
    item_data <- keep_t(item_data, ctrl)
    item_data <- keep_geo(item_data, ctrl)
    drop_responseless_items(item_data, ctrl)
    drop_items_rare_in_time(item_data, ctrl)
    drop_items_rare_in_polls(item_data, ctrl)
    final_dim <- dim(item_data)
    iter <- iter + 1L
    if (identical(initial_dim, final_dim)) {
      message("\tNo changes")
    } else {
      # FIXME: 
      message("\tRemaining: ", format(nrow(item_data), big.mark = ","), " rows, ",
              length(intersect(ctrl@item_names, names(item_data))), " items")
    }
  }
  setkeyv(item_data, c(ctrl@geo_name, ctrl@time_name))
  invisible(item_data)
}

restrict_modifier <- function(item_data, modifier_data, ctrl) {
  if (length(modifier_data)) {
    setDT(modifier_data)

    coerce_factors(modifier_data, c(ctrl@modifier_names,
                                    ctrl@t1_modifier_names,
                                    ctrl@geo_name,
                                    ctrl@time_name))

    modifier_data <- modifier_data[modifier_data[[ctrl@geo_name]] %in%
                                   item_data[[ctrl@geo_name]] &
                                 modifier_data[[ctrl@time_name]] %in%
                                   item_data[[ctrl@time_name]]]

    if (!identical(nrow(modifier_data),
                   nrow(unique(modifier_data[, c(ctrl@geo_name, ctrl@time_name), with = FALSE])))) {
      stop("time and geo identifiers don't uniquely identify modifier data observations")
    }
    message()
    message("Restricted modifier data to time and geo observed in item data.")
  }
  invisible(modifier_data)
}

restrict_aggregates <- function(aggregate_data, ctrl) {
  if (length(aggregate_data)) {
    setDT(aggregate_data)
    aggregate_data <- subset(aggregate_data,
                             aggregate_data[[ctrl@geo_name]] %chin% ctrl@geo_filter & 
                             aggregate_data[[ctrl@time_name]] %in% ctrl@time_filter &
                             aggregate_data[["item"]] %in% ctrl@aggregate_item_names)
    # subset to observed; FIXME: assumes n_grp variable name
    aggregate_data <- aggregate_data[n_grp > 0]
    extra_colnames <- setdiff(names(aggregate_data),
                              c(ctrl@geo_name, ctrl@time_name, ctrl@group_names, "item", "s_grp", "n_grp"))
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

rename_numerics <- function(item_data, ctrl) {
  varnames <- c(ctrl@group_names, ctrl@geo_name, ctrl@modifier_names,
                ctrl@t1_modifier_names)
  varnames <- varnames[vapply(item_data[, varnames], is.numeric, logical(1))]
  if (length(varnames)) {
    for (v in varnames) {
      item_data[, (v) := paste0(v, item_data[[v]])]
    }
  }
  invisible(item_data)
}

drop_rows_missing_covariates <- function(item_data, ctrl) {
  n <- nrow(item_data)
  is_missing <- rowSums(is.na(item_data[, c(ctrl@geo_name, ctrl@time_name, ctrl@group_names, ctrl@survey_name), with = FALSE])) > 0
  item_data <- subset(item_data, !is_missing)
  if (!identical(n, nrow(item_data))) {
    message("\tDropped ", format(n - nrow(item_data), big.mark = ","),
            " rows for missingness in covariates")
  }
  item_data
}

with_contr.treatment <- function(...) {
  contrast_options = getOption("contrasts")
  options("contrasts"= c(unordered = "contr.treatment",
                         ordered = "contr.treatment"))
  res <- eval(...)
  options("contrasts"= contrast_options)
  res
}

keep_t <- function(item_data, ctrl) {
  item_data <- item_data[get(ctrl@time_name) %in% ctrl@time_filter]
  invisible(item_data)
}

keep_geo <- function(item_data, ctrl) {
  item_data <- item_data[get(ctrl@geo_name) %chin% ctrl@geo_filter]
  invisible(item_data)
}

drop_responseless_items <- function(item_data, ctrl) {
  item_names <- intersect(ctrl@item_names, names(item_data))
  response_n <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) == 0),
                          .SDcols = item_names]
  responseless_items <- melt.data.table(response_n, id.vars = NULL, measure.vars
                                        = names(response_n))[(value)]
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

drop_items_rare_in_time <- function(item_data, ctrl) {
  item_names <- intersect(ctrl@item_names, names(item_data))
  setkeyv(item_data, item_data[, ctrl@time_name])
  response_t <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) > 0), .SDcols
                          = item_names, by = eval(item_data[, ctrl@time_name])]
  response_t <- melt.data.table(response_t, id.vars = ctrl@time_name)[(value)]
  response_t <- response_t[, N := .N, by = variable]
  response_t <- response_t[N < ctrl@min_t_filter]
  rare_items <- as.character(response_t[, variable])
  if (length(rare_items)) {
    for (v in rare_items) {
      item_data[, (v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_t requirement (%i)",
          "\tDropped %i items for failing min_t requirement (%i)"),
        length(rare_items), ctrl@min_t_filter))
  }
  invisible(item_data)
}

drop_items_rare_in_polls <- function(item_data, ctrl) {
  item_names <- intersect(ctrl@item_names, names(item_data))
  #TODO: dedupe; cf. drop_items_rare_in_time
  setkeyv(item_data, item_data[, ctrl@survey_name])
  item_survey <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) > 0),
                           .SDcols = item_names,
                           by = eval(item_data[, ctrl@survey_name])]
  item_survey <- melt.data.table(item_survey, id.vars =
                                 ctrl@survey_name)[(value)]
  item_survey <- item_survey[, N := .N, by = variable]
  item_survey <- item_survey[N < ctrl@min_survey_filter]
  rare_items <- as.character(item_survey[, variable])
  if (length(rare_items)) {
    for (v in rare_items) {
      item_data[, (v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_survey requirement (%i)",
          "\tDropped %i items for failing min_survey requirement (%i)"),
        length(rare_items), ctrl@min_survey_filter))
  }
  invisible(item_data)
}

make_group_counts <- function(item_data, aggregate_data, d_in, ctrl) {
  # Make a table giving success and trial counts by group and item
  item_data[, ("n_responses") := list(rowSums(!is.na(.SD))),
            .SDcols = d_in$gt_items]
  item_data[, ("def") := lapply(.SD, calc_design_effects),
            .SDcols = ctrl@weight_name, with = FALSE,
            by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

  # get design-effect-adjusted nonmissing response counts by group and item
  item_n <- item_data[, lapply(.SD, count_items, get("n_responses"), get("def")),
                      .SDcols = c(d_in$gt_items),
                      by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

  # append _n_grp to the response count columns
  item_n_vars <- paste0(ctrl@item_names, "_n_grp")
  names(item_n) <- replace(names(item_n), match(d_in$gt_items, names(item_n)), item_n_vars)
  setkeyv(item_n, c(ctrl@time_name, ctrl@geo_name, ctrl@group_names))

  item_data[, ("adj_weight") := get(ctrl@weight_name) / n_responses]
  item_means <- item_data[, lapply(.SD, function(x) weighted.mean(x, .SD$adj_weight, na.rm = TRUE)),
                          .SDcols = c(d_in$gt_items, "adj_weight"),
                          by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

  # append _mean to the mean response columns 
  item_mean_vars <- paste0(ctrl@item_names, "_mean")
  names(item_means) <- replace(names(item_means), match(d_in$gt_items, names(item_means)), item_mean_vars)
  setkeyv(item_means, c(ctrl@time_name, ctrl@geo_name, ctrl@group_names))

  # join response counts with means 
  counts_means <- item_n[item_means]
  counts_means <- counts_means[, c(ctrl@time_name, ctrl@geo_name,
                                   ctrl@group_names, item_mean_vars,
                                   item_n_vars), with = FALSE]

  # the group success count for an item is the product of its count and mean
  item_s_vars <- paste0(ctrl@item_names, "_s_grp")
  counts_means[, (item_s_vars) := round(counts_means[, (item_mean_vars), with = FALSE] * counts_means[, (item_n_vars), with = FALSE], 0)]
  counts_means <- counts_means[, -grep("_mean$", names(counts_means)), with = FALSE]

  # we want a long table of successes (s_grp) and trials (n_grp) by group and
  # item; items need to move from columns to rows
  melted <- melt(counts_means, id.vars = c(ctrl@time_name, ctrl@geo_name, ctrl@group_names), variable.name = "item")
  melted[, c("variable", "item") := list(gsub(".*([sn]_grp)$", "\\1", item), gsub("(.*)_[sn]_grp$", "\\1", item))]
  f <- as.formula(paste0(paste(ctrl@time_name, ctrl@geo_name, ctrl@group_names, "item", sep = "+"), "~variable"))
  group_counts <- data.table::dcast.data.table(melted, f)

  # stan code expects unobserved group-items to be omitted
  group_counts <- group_counts[n_grp != 0 & !is.na(n_grp)]
  # be sure to represent no observed success with a zero count, not NA
  group_counts[is.na(s_grp), s_grp := 0]

  # include aggregates, if any
  if (length(aggregate_data) && nrow(aggregate_data) > 0) {
    message("Added ", length(ctrl@aggregate_item_names), " items from aggregate data.")
    group_counts <- rbind(group_counts, aggregate_data)
  }
  group_counts
}

count_items <- function(x, n_responses, def) {
  ceiling(sum(as.integer(!is.na(x)) / n_responses / def, na.rm = TRUE))
}

get_missing_groups <- function(group_counts, group_grid, ctrl) {
  all_group_counts <- merge(group_counts, group_grid, all = TRUE,
                            by = c(ctrl@group_names, ctrl@geo_name, ctrl@time_name))
  all_group_counts[, ("is_missing") := is.na(n_grp) + 0L]
  all_group_counts[is.na(n_grp), c("n_grp", "s_grp") := 1L]
  all_group_counts[, (ctrl@geo_name) := paste0("x_", .SD[[ctrl@geo_name]]), .SDcols = c(ctrl@geo_name)]
  acast_formula <- as.formula(paste0(ctrl@time_name, "~ item ~", paste(ctrl@group_names, collapse = "+"), "+", ctrl@geo_name))
  MMM <- reshape2::acast(all_group_counts, acast_formula, value.var = "is_missing", fill = 1)
  # merging group_grid included unobserved combinations of grouping variables; being unobserved, they're associated with
  # no item, and when the result is cast to array, NA will appear in the second dimension as an item name
  MMM <- MMM[, dimnames(MMM)[[2]] != "NA", , drop = FALSE]
  stopifnot(all(MMM %in% c(0, 1)))
  MMM
}

shape_hierarchical_data <- function(item_data, modifier_data, d_in, ctrl) {
  if (d_in$G_hier == 1) {
    zz.names <- list(ctrl@time_filter, dimnames(d_in$XX)[[2]], "")
    zz <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  } else {
    # the array of hierarchical data ZZ should be T x P x H, where T is the number of time periods, P is the number of
    # hierarchical parameters (including the geographic), and H is the number of predictors for geographic unit effects
    # TODO: make flexible; as written we must model geo x t
    modeled_params = c(ctrl@geo_name, ctrl@time_name)
    unmodeled_params = setdiff(c(ctrl@geo_name, ctrl@time_name, ctrl@group_names), modeled_params)

    # does hierarchical data include all observed geo?
    missing_modifier_geo <- setdiff(
                                    unique(d_in$group_grid_t[[ctrl@geo_name]]),
                                    unique(modifier_data[[ctrl@geo_name]]))
    if (length(missing_modifier_geo)) stop("no hierarchical data for geo in item data: ", missing_modifier_geo)
    # does hierarchical data include all modeled t?
    missing_modifier_t <- setdiff(ctrl@time_filter, unique(modifier_data[[ctrl@time_name]]))
    if (length(missing_modifier_t)) stop("missing hierarchical data for t in item data: ", missing_modifier_t)

    # NOTE: we're prepending the value of geo with its variable name; may not be desirable
    hier_frame <- copy(modifier_data)[, ctrl@geo_name := paste0(ctrl@geo_name, modifier_data[[ctrl@geo_name]])]
    hier_frame[, setdiff(names(hier_frame), c(modeled_params, modifier_names)) := NULL, with = FALSE]
    hier_frame[, c("param", ctrl@geo_name) := .(hier_frame[[ctrl@geo_name]], NULL), with = FALSE]
    setkeyv(hier_frame, c("param", ctrl@time_name))

    modeled_param_names <- unique(hier_frame[, param])
    unmodeled_param_levels = unlist(lapply(unmodeled_params, function(x) {
                                             paste0(x, unique(d_in$group_grid_t[[x]]))[-1]
        }))
    param_levels <- c(modeled_param_names, unmodeled_param_levels)

    unmodeled_frame <- expand.grid(c(list(
                                          unmodeled_param_levels,
                                          sort(unique(hier_frame[[ctrl@time_name]])),
                                          as.integer(rep(list(0), length(modifier_names))))))
    unmodeled_frame <- setNames(unmodeled_frame, c("param", ctrl@time_name, modifier_names))
    setDT(unmodeled_frame, key = c("param", ctrl@time_name))

    hier_frame <- rbind(hier_frame, unmodeled_frame)

    # FIXME: hacky handling of the possibility of NA modifier values here
    hier_frame[, c(modifier_names) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = modifier_names]

    hier_melt = melt(hier_frame, id.vars = c("param", ctrl@time_name), variable.name = "modifiers", variable.factor = FALSE,
                     value.factor = FALSE)
    setkeyv(hier_melt, c("param", "year"))

    melt_formula <- as.formula(paste(ctrl@time_name, "param", "modifiers", sep = " ~ "))
    zz <- reshape2::acast(hier_melt, melt_formula, drop = FALSE, value.var = "value")
    zz <- zz[, -1, , drop = FALSE]
  } 
  zz
}

make_design_matrix <- function(item_data, d_in, ctrl) {
  design_formula <- as.formula(paste("~ 0", ctrl@geo_name,
                                     paste(ctrl@group_names, collapse = " + "),
                                     sep = " + "))
  design_matrix <- with_contr.treatment(model.matrix(design_formula, d_in$group_grid_t))
  rownames(design_matrix) <- paste(paste(d_in$group_grid_t[[ctrl@group_names]], sep = "_"),
                                   d_in$group_grid_t[[ctrl@geo_name]],  sep = "_x_")
  design_matrix <- subset(design_matrix, select = -1)
  # TODO: move to S4 validate
  invalid_values <- setdiff(as.vector(design_matrix), c(0, 1))
  if (length(invalid_values)) {
    stop("design matrix values should be in (0, 1); found ",
         paste(sort(invalid_values), collapse = ", "))
  }
  design_matrix
}

calc_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}
