make_group_grid <- function(item_data, aggregate_data, ctrl) {
  # Make a table giving combinations of the grouping variables
  group_grid <- expand.grid(c(
    setNames(list(ctrl@time_filter), ctrl@time_name),
    lapply(rbind(item_data[, c(ctrl@geo_name, ctrl@group_names), with = FALSE],
                 aggregate_data[, c(ctrl@group_names, ctrl@geo_name), with = FALSE]),
           function(x) sort(unique(x)))), stringsAsFactors = FALSE)
  data.table::setDT(group_grid, key = c(ctrl@group_names, ctrl@time_name, ctrl@geo_name))
  invisible(group_grid)
}

make_group_grid_t <- function(group_grid, ctrl) {
  # Make a table giving combinations of grouping variables, excluding time
  group_grid_t <- data.table::copy(group_grid)[, ctrl@time_name := NULL]
  group_grid_t <- group_grid_t[!duplicated(group_grid_t)]
  data.table::setkeyv(group_grid_t, c(ctrl@group_names, ctrl@geo_name))
  group_grid_t
}

make_group_counts <- function(item_data, aggregate_data, ctrl) {
  # Make a table giving success and trial counts by group and item.
  #
  # Because of how DGIRT Stan code iterates over the data, the result must be
  # ordered by time, item, and then group. The order of the grouping variables
  # doesn't matter.
  if (length(item_data)) {
    gt_names <- attr(item_data, "gt_items")
    item_data[, c("n_gt_responses") := list(rowSums(!is.na(.SD))),
              .SDcols = gt_names]
    item_data[, c("n_item_responses") := list(rowSums(!is.na(.SD))),
              .SDcols = ctrl@item_names]

    if (!length(ctrl@weight_name)) {
      item_data[, weight := 1L]
      ctrl@weight_name <- "weight"
    }

    item_data[, c("def") := lapply(.SD, calc_design_effects),
              .SDcols = ctrl@weight_name,
              by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

    # get the number of dichotomized item responses for each item (1 for a
    # dichotomous item, or K-1) 
    item_levels = vapply(ctrl@item_names, function(item_name) {
      sum(grepl(paste0('^', item_name, '_gt[0-9]+$'), gt_names))
    }, integer(1))

    # weight each _gt response: divide it by the number of _gt variables for the
    # item (for an item already dichotomous, this is 1), and then divide it by
    # the number of item responses
    for (item_name in ctrl@item_names) {
      for (gt_item in gt_names[grepl(paste0('^', item_name, '_gt[0-9]+$'), gt_names)]) {
        item_data = item_data[, paste0(gt_item, '_weighted') := get(gt_item) /
          item_levels[item_name] / get('n_item_responses')]
      }
    }
    # get design-effect-adjusted nonmissing response counts by group and item
    gt_wtd_names = paste0(gt_names, '_weighted')
    item_n <- item_data[, lapply(.SD, count_items, get("def")),
      .SDcols = c(gt_wtd_names), by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]
    item_n <- melt(item_n, id.vars = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name),
      variable.name = 'item', value.name = 'n_grp')
    item_n[, item := sub('_weighted', '', item)]

    # take the weighted average over the *previously weighted* _gt responses,
    # where the weights for the average are individual weights divided by item
    # responses 
    item_data[, c("adj_weight") := get(ctrl@weight_name) / get("n_item_responses")]
    item_means <- item_data[, lapply(.SD, function(x) weighted.mean(x,
        .SD$adj_weight, na.rm = TRUE)), .SDcols = c(gt_wtd_names, "adj_weight"),
      by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

    item_mean_vars <- paste0(gt_wtd_names, "_mean")
    setnames(item_means, gt_wtd_names, item_mean_vars)
    data.table::setkeyv(item_means, c(ctrl@time_name, ctrl@geo_name, ctrl@group_names))
    drop_cols <- setdiff(names(item_means), c(key(item_means), item_mean_vars))
    if (length(drop_cols)) {
      item_means[, c(drop_cols) := NULL]
    }
    item_means <- melt(item_means, id.vars = c(ctrl@geo_name, ctrl@group_names,
        ctrl@time_name), variable.name = 'item', value.name = 'item_mean')
    item_means[, item := sub('_weighted_mean', '', item)]

    stopifnot(!length(setdiff(item_n$item, item_means$item)))
    stopifnot(!length(setdiff(item_means$item, item_n$item)))
    counts = merge(item_n, item_means, by = c(ctrl@time_name, ctrl@geo_name,
        ctrl@group_names, "item"), all.x = TRUE)
    counts[, s_grp := round(n_grp * item_mean, 0)]
    counts[, item_mean := NULL]
    stopifnot(all(counts$s_grp <= counts$n_grp))
    stopifnot(!any(is.na(counts$s_grp)))
    stopifnot(!any(is.na(counts$n_grp)))
    counts[]
  }

  # include aggregates, if any
  if (length(item_data) && length(aggregate_data) && nrow(aggregate_data) > 0) {
    # invariant: we have both individual- and aggregate-level item responses
    counts <- data.table::rbindlist(list(counts, aggregate_data), use.names =
                                    TRUE)
    message("Added ", length(ctrl@aggregate_item_names), " items from aggregate data.")
  } else if (length(aggregate_data) && nrow(aggregate_data) > 0) {
    # invariant: we have only aggregate-level item responses
    # aggregate_data is already in the expected format
    counts <- aggregate_data
    message("Using ", length(ctrl@aggregate_item_names), " items from aggregate data.")
  } else if (!length(item_data)) {
    # invariant: we unexpectedly have neither individual- nor aggregate-level data
    stop("can't proceed with neither item_data nor aggregate_data")
  }

  data.table::setkeyv(counts, c(ctrl@time_name, "item", ctrl@group_names,
      ctrl@geo_name))

  # include unobserved cells
  all_groups = expand.grid(c(setNames(list(unique(counts[[ctrl@geo_name]])), ctrl@geo_name),
                             setNames(list(ctrl@time_filter), ctrl@time_name),
                             lapply(counts[, c(ctrl@group_names,
                                                     "item"), with = FALSE],
                                    function(x) sort(unique(x)))),
                           stringsAsFactors = FALSE)
  counts <- merge(counts, all_groups, all = TRUE)

  # unobserved cells should be zeroed
  counts[is.na(get("s_grp")), c("s_grp") := 0]
  counts[is.na(get("n_grp")), c("n_grp") := 0]

  # create an identifier for use in n_vec and s_vec
  counts[, c("name") := do.call(paste, c(.SD, sep = "__")), .SDcols =
               c(ctrl@time_name, ctrl@geo_name, ctrl@group_names, "item")]

  setkeyv(counts, c(ctrl@time_name, "item", ctrl@group_names, ctrl@geo_name))
  counts
}

count_items <- function(x, def) {
  ceiling(sum(as.integer(!is.na(x)) / def, na.rm = TRUE))
}

calc_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}
