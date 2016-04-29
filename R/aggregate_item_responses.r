make_group_grid <- function(item_data, aggregate_data, ctrl) {
  # Make a table giving combinations of the grouping variables
  group_grid <- expand.grid(c(
    setNames(list(ctrl@time_filter), ctrl@time_name),
    lapply(rbind(item_data[, c(ctrl@geo_name, ctrl@group_names), with = FALSE],
                 aggregate_data[, c(ctrl@group_names, ctrl@geo_name), with =
                                FALSE]),
           function(x) sort(unique(x)))), stringsAsFactors = FALSE)
  data.table::setDT(group_grid, key = c(ctrl@time_name, ctrl@group_names, ctrl@geo_name))
  invisible(group_grid)
}

make_group_grid_t <- function(group_grid, ctrl) {
  # Make a table giving combinations of grouping variables, excluding time
  group_grid_t <- data.table::copy(group_grid)[, ctrl@time_name := NULL, with = FALSE]
  group_grid_t <- group_grid_t[!duplicated(group_grid_t)]
  setkeyv(group_grid_t, c(ctrl@group_names, ctrl@geo_name))
  group_grid_t
}

make_group_counts <- function(item_data, aggregate_data, d_in, ctrl) {
  # Make a table giving success and trial counts by group and item.
  #
  # Because of how DGIRT Stan code iterates over the data, the result must be
  # ordered by time, item, and then group. The order of the grouping variables
  # doesn't matter so long as it's consistent between here and MMM.
  item_data[, c("n_responses") := list(rowSums(!is.na(.SD))),
            .SDcols = d_in$gt_items]
  item_data[, c("def") := lapply(.SD, calc_design_effects),
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

  item_data[, c("adj_weight") := get(ctrl@weight_name) / get("n_responses")]
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
  counts_means[, c(item_s_vars) := round(counts_means[, (item_mean_vars), with = FALSE] * counts_means[, (item_n_vars), with = FALSE], 0)]
  counts_means <- counts_means[, -grep("_mean$", names(counts_means)), with = FALSE]

  # we want a long table of successes (s_grp) and trials (n_grp) by group and
  # item; items need to move from columns to rows
  melted <- melt(counts_means, id.vars = c(ctrl@time_name, ctrl@geo_name, ctrl@group_names), variable.name = "item")
  melted[, c("variable", "item") := list(gsub(".*([sn]_grp)$", "\\1", get("item")),
                                         gsub("(.*)_[sn]_grp$", "\\1", get("item")))]
  f <- as.formula(paste0(paste(ctrl@time_name, ctrl@geo_name, paste(ctrl@group_names, collapse = " + "),
                               "item", sep = "+"), " ~ variable"))
  group_counts <- data.table::dcast.data.table(melted, f)

  # stan code expects unobserved group-items to be omitted
  group_counts <- group_counts[get("n_grp") != 0 & !is.na(get("n_grp"))]
  # be sure to represent no observed success with a zero count, not NA
  group_counts[is.na(get("s_grp")), c("s_grp") := 0]

  # include aggregates, if any
  if (length(aggregate_data) && nrow(aggregate_data) > 0) {
    group_counts <- rbind(group_counts, aggregate_data)
    message("Added ", length(ctrl@aggregate_item_names), " items from aggregate data.")
  }

  # create an identifier for use in n_vec and s_vec 
  group_counts[, c("name") := do.call(paste, c(.SD, sep = "__")), .SDcols =
               c(ctrl@time_name, ctrl@geo_name, ctrl@group_names, "item")]

  setkeyv(group_counts, c(ctrl@time_name, "item", ctrl@group_names, ctrl@geo_name))
  group_counts
}

count_items <- function(x, n_responses, def) {
  ceiling(sum(as.integer(!is.na(x)) / n_responses / def, na.rm = TRUE))
}
