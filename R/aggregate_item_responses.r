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
    if (!length(ctrl@weight_name)) {
      item_data[, weight := 1L]
      ctrl@weight_name <- "weight"
    }

    group_names = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)
    # Calculate n* and s* by group 
    ns = by(item_data, item_data[, c(group_names), with = FALSE],
      function(group_data) {
        stopifnot(is.data.frame(group_data))
        r <- calc_r(group_data, gt_names)
        nstar <- calc_nstar(group_data, r, ctrl, gt_names)
        sstar <- calc_sstar(group_data, r, ctrl, nstar, gt_names)
        # Move results into dataframes with columns n_grp/s_grp, 'item' (e.g.,
        # 'abortion_gt1'), and the grouping variables
        item_n = data.table(n_grp = nstar, item = names(nstar),
          group_data[1, c(group_names), with = FALSE])
        item_s = data.table(s_grp = sstar, item = names(sstar),
          group_data[1, c(group_names), with = FALSE])
        stopifnot(!length(setdiff(item_n$item, item_s$item)))
        stopifnot(!length(setdiff(item_s$item, item_n$item)))
        # The result of the merge is a dataframe with group identifiers and
        # columns n_grp, s_grp, and item
        ns = merge(item_s, item_n, by = c('item', group_names), all = TRUE)
        ns
    }, simplify = FALSE)
    counts = rbindlist(ns, use.names = TRUE)
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
  sum(as.integer(!is.na(x)) / def, na.rm = TRUE)
}

calc_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}

grep_gt <- function(df) {
  grep("\\_gt[0-9]+$", names(df), value = TRUE)
}

calc_r <- function(item_dt, gt_names) {
  item_df <- as.data.frame(item_dt)
  (gt_stems <- (gsub("(.*)\\_gt[0-9]+$", "\\1", gt_names)))
  r <- matrix(NA, nrow = nrow(item_dt), ncol = length(gt_names),
    dimnames = list(1:nrow(item_dt), gt_names))
  for (i in 1:nrow(item_df)) {
    (valid_i <- !is.na(item_df[i, gt_names]))
    (n_qs <- sum(tapply(valid_i, gt_stems, any)))
    stem_sums <- tapply(valid_i, gt_stems, sum)
    n_dups <- stem_sums[match(gt_stems, names(stem_sums))]
    names(n_dups) <- gt_names 
    (r[i, ] <- n_qs * n_dups)
    stopifnot(identical(sum(as.vector(valid_i) / r[i, ], na.rm = TRUE), 1))
  }
  r
}

calc_nstar <- function (item_subset, r_subset, ctrl, gt_names) {
  item_subset <- as.data.frame(item_subset)
  wt <- item_subset[, ctrl@weight_name]
  def <- 1 + (sd(wt) / mean(wt))
  n_mat <- as.matrix(!is.na(item_subset[, gt_names])) / (r_subset * def)
  ceiling(colSums(n_mat, na.rm = TRUE))
}

calc_sstar <- function (item_subset, r_subset, ctrl, nstar, gt_names) {
  item_subset <- as.data.frame(item_subset)
  wt <- item_subset[, ctrl@weight_name]
  ystar <- numeric(length(gt_names))
  for (j in seq_along(gt_names)) {
    ystar[j] <- weighted.mean(item_subset[, gt_names[j]],
      w = wt / r_subset[, j],
      na.rm = TRUE)
  }
  sstar <- round(ystar * nstar)
  replace(sstar, is.na(sstar), 0)
}

