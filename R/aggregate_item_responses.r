make_group_grid <- function(item_data, aggregate_data, ctrl) {
  # Make a table giving combinations of the grouping variables
  group_grid <- expand.grid(c(
    setNames(list(ctrl@time_filter), ctrl@time_name),
    lapply(rbind(item_data[, c(ctrl@geo_name, ctrl@group_names), with = FALSE],
                 aggregate_data[, c(ctrl@group_names, ctrl@geo_name), with =
                                FALSE]),
           function(x) sort(unique(x)))), stringsAsFactors = FALSE)
  data.table::setDT(group_grid, key = c(ctrl@group_names, ctrl@time_name, ctrl@geo_name))
  invisible(group_grid)
}

make_group_grid_t <- function(group_grid, ctrl) {
  # Make a table giving combinations of grouping variables, excluding time
  group_grid_t <- data.table::copy(group_grid)[, ctrl@time_name := NULL, with = FALSE]
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
  gt_names <- attr(item_data, "gt_items")
  item_data[, c("n_responses") := list(rowSums(!is.na(.SD))),
            .SDcols = gt_names]
  item_data[, c("def") := lapply(.SD, calc_design_effects),
            .SDcols = ctrl@weight_name, with = FALSE,
            by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

  # get design-effect-adjusted nonmissing response counts by group and item
  item_n <- item_data[, lapply(.SD, count_items, get("n_responses"), get("def")),
                      .SDcols = c(gt_names),
                      by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]
  # append _n_grp to the response count columns
  item_n_vars <- paste0(gt_names, "_n_grp")
  names(item_n) <- replace(names(item_n), match(gt_names, names(item_n)), item_n_vars)
  data.table::setkeyv(item_n, c(ctrl@time_name, ctrl@geo_name, ctrl@group_names))
  drop_cols <- setdiff(names(item_n), c(key(item_n), item_n_vars))
  if (length(drop_cols)) {
    item_n[, c(drop_cols) := NULL, with = FALSE]
  }

  # get mean ystar
  item_data[, c("adj_weight") := get(ctrl@weight_name) / get("n_responses")]
  item_means <- item_data[, lapply(.SD, function(x) weighted.mean(x, .SD$adj_weight, na.rm = TRUE)),
                          .SDcols = c(gt_names, "adj_weight"),
                          by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]
  # append _mean to the mean response columns 
  item_mean_vars <- paste0(gt_names, "_mean")
  names(item_means) <- replace(names(item_means), match(gt_names, names(item_means)), item_mean_vars)
  data.table::setkeyv(item_means, c(ctrl@time_name, ctrl@geo_name, ctrl@group_names))
  drop_cols <- setdiff(names(item_means), c(key(item_means), item_mean_vars))
  item_means[, c(drop_cols) := NULL, with = FALSE]

  # join response counts with means 
  count_means <- item_n[item_means]
  count_means <- count_means[, c(ctrl@time_name, ctrl@geo_name,
                                   ctrl@group_names, item_mean_vars,
                                   item_n_vars), with = FALSE]

  # the group success count for an item is the product of its count and mean
  item_s_vars <- paste0(gt_names, "_s_grp")
  count_means[, c(item_s_vars) := round(count_means[, (item_mean_vars), with = FALSE] *
                                         count_means[, (item_n_vars), with = FALSE], 0)]
  count_means <- count_means[, -grep("_mean$", names(count_means)), with = FALSE]


  # we want a long table of successes (s_grp) and trials (n_grp) by group and
  # item; items need to move from columns to rows
  melted <- melt(count_means, id.vars = c(ctrl@time_name, ctrl@geo_name,
                                           ctrl@group_names),
                 variable.name = "item")
  melted[, c("variable") := list(gsub(".*([sn]_grp)$", "\\1", get("item")))]
  melted[, c("item") := list(gsub("(.*)_[sn]_grp$", "\\1", get("item")))]
  f <- as.formula(paste0(paste(ctrl@time_name, ctrl@geo_name,
                               paste(ctrl@group_names, collapse = " + "),
                               "item", sep = "+"), " ~ variable"))
  counts <- data.table::dcast.data.table(melted, f, drop = FALSE, fill = 0L)

  # include aggregates, if any
  if (length(aggregate_data) && nrow(aggregate_data) > 0) {
    counts <- data.table::rbindlist(list(counts, aggregate_data), use.names =
                                    TRUE)
    message("Added ", length(ctrl@aggregate_item_names), " items from aggregate data.")
    data.table::setkeyv(counts, c(ctrl@time_name, "item", ctrl@group_names,
                                  ctrl@geo_name))
  }

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

count_items <- function(x, n_responses, def) {
  ceiling(sum(as.integer(!is.na(x)) / n_responses / def, na.rm = TRUE))
}
