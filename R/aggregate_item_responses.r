make_group_grid <- function(item_data, aggregate_data, ctrl) {
  # Make a table giving combinations of the grouping variables
  group_grid <- expand.grid(c(
    setNames(list(ctrl@time_filter), ctrl@time_name),
    lapply(rbind(item_data[, c(ctrl@geo_name, ctrl@group_names), with = FALSE],
                 aggregate_data[, c(ctrl@group_names, ctrl@geo_name), with =
                                FALSE]),
           function(x) sort(unique(x)))), stringsAsFactors = FALSE)
  data.table::setDT(group_grid, key = c(ctrl@group_names, ctrl@time_name,
                                        ctrl@geo_name))
  invisible(group_grid)
}

make_group_grid_t <- function(group_grid, ctrl) {
  # Make a table giving combinations of grouping variables, excluding time
  group_grid_t <- data.table::copy(group_grid)[, ctrl@time_name := NULL, with =
                                               FALSE]
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
  group_split <- split(item_data, by = c(ctrl@geo_name, ctrl@group_names,
                                         ctrl@time_name), keep.by = TRUE)
  counts <- data.table::rbindlist(lapply(group_split, function(x) {
               agg_items(x[, gt_names, with = FALSE],
                         x[["n_responses"]], x[[ctrl@weight_name]])
    }), id = TRUE)

  counts = counts[, c(ctrl@geo_name, ctrl@group_names, ctrl@time_name) :=
                      tstrsplit(.id, ".", fixed = TRUE), with = FALSE]
  counts[, c(".id") := NULL]
  counts[, c(ctrl@time_name) := .(type.convert(get(ctrl@time_name))), with =
         FALSE]
  setkeyv(counts, c(ctrl@geo_name, ctrl@group_names, ctrl@time_name, "item"))

  # include aggregates, if any
  if (length(aggregate_data) > 0 && nrow(aggregate_data) > 0) {
    counts <- data.table::rbindlist(list(counts, aggregate_data), use.names =
                                    TRUE)
    message("Added ", length(ctrl@aggregate_item_names), " items from aggregate data.")
    data.table::setkeyv(counts, c(ctrl@time_name, "item", ctrl@group_names,
                                  ctrl@geo_name))
  }

  # include unobserved cells
  all_groups = expand.grid(c(setNames(list(ctrl@geo_filter), ctrl@geo_name),
                             setNames(list(ctrl@time_filter), ctrl@time_name),
                             "item" = list(gt_names),
                             lapply(item_data[, ctrl@group_names, with = FALSE],
                                    function(x) sort(unique(x)))),
                           stringsAsFactors = FALSE)
  counts <- merge(counts, all_groups, all = TRUE,
                   by = c(ctrl@geo_name, ctrl@time_name, ctrl@group_names,
                          "item"))
  counts[is.na(get("s_grp")), c("s_grp") := 0]
  counts[is.na(get("n_grp")), c("n_grp") := 0]

  # create an identifier for use in n_vec and s_vec 
  counts[, c("name") := do.call(paste, c(.SD, sep = "__")), .SDcols =
               c(ctrl@time_name, ctrl@geo_name, ctrl@group_names, "item")]

  setkeyv(counts, c(ctrl@time_name, "item", ctrl@group_names, ctrl@geo_name))
  counts
}
