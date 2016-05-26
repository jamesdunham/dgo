test_that("group aggregates via Rcpp match original implementation's", {
  # library(Rcpp)
  # library(data.table)
  # sourceCpp('~/projects/dgirt/src/aggregate_items.cpp')

  # rcpp way
  setup <- function() {
    data(opinion)
    item_data <- opinion
    setDT(item_data)
    gt_names <- c("Q_cces2006_abortion", "Q_cces2006_gaymarriageamendment")
    item_data <- item_data[!is.na(race) & !is.na(weight)]
    item_data[, c("n_responses") := list(rowSums(!is.na(.SD))), .SDcols = gt_names]
    item_data <- item_data[!is.na(race)] 
    ctrl <- dgirt:::init_control(item_data, item_names = gt_names,
                                 time_name = "year", geo_name = "state",
                                 group_names = "race", survey_name = "source",
                                 weight_name = "weight")
    item_data <- item_data[year %in% ctrl@time_filter]
    item_data <- item_data[state %in% ctrl@geo_filter]
    setattr(item_data, "gt_items", gt_names)
    attr(item_data, "gt_items")
    return(list(item_data, ctrl))
  }
  s <- setup()
  cpp_out <- make_group_counts(s[[1]], NULL, s[[2]])

  old_make_group_counts <- function(item_data, aggregate_data, ctrl) {
    # Make a table giving success and trial counts by group and item.
    #
    # Because of how DGIRT Stan code iterates over the data, the result must be
    # ordered by time, item, and then group. The order of the grouping variables
    # doesn't matter so long as it's consistent between here and MMM.
    gt_names <- attr(item_data, "gt_items")
    item_data[, c("n_responses") := list(rowSums(!is.na(.SD))),
              .SDcols = gt_names]

    calc_design_effects <- function(x) {
      y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
      ifelse(is.na(y), 1, y)
    }

    item_data[, c("def") := lapply(.SD, calc_design_effects),
              .SDcols = ctrl@weight_name, with = FALSE,
              by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

    count_items <- function(x, n_responses, def) {
      ceiling(sum(as.integer(!is.na(x)) / n_responses / def, na.rm = TRUE))
    }

    # get design-effect-adjusted nonmissing response counts by group and item
    item_n <- item_data[, lapply(.SD, count_items, n_responses, def),
                        .SDcols = gt_names,
                        by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]
    # append _n_grp to the response count columns
    item_n_vars <- paste0(gt_names, "_n_grp")
    names(item_n) <- replace(names(item_n), match(gt_names, names(item_n)), item_n_vars)
    data.table::setkeyv(item_n, c(ctrl@time_name, ctrl@geo_name, ctrl@group_names))
    drop_cols <- setdiff(names(item_n), c(key(item_n), item_n_vars))
    if (length(drop_cols))
      item_n[, c(drop_cols) := NULL, with = FALSE]

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
    all_groups = expand.grid(c(setNames(list(ctrl@geo_filter), ctrl@geo_name),
                               setNames(list(ctrl@time_filter), ctrl@time_name),
                               lapply(counts[, c(ctrl@group_names,
                                                       "item"), with = FALSE],
                                      function(x) sort(unique(x)))),
                             stringsAsFactors = FALSE)
    counts <- merge(counts, all_groups, all = TRUE)

    counts[is.na(get("s_grp")), c("s_grp") := 0]
    counts[is.na(get("n_grp")), c("n_grp") := 0]

    # create an identifier for use in n_vec and s_vec 
    counts[, c("name") := do.call(paste, c(.SD, sep = "__")), .SDcols =
                 c(ctrl@time_name, ctrl@geo_name, ctrl@group_names, "item")]

    setkeyv(counts, c(ctrl@time_name, "item", ctrl@group_names, ctrl@geo_name))
    counts
  }

  # old way
  s <- setup()
  r_out <- old_make_group_counts(s[[1]], NULL, s[[2]])

  cpp_out[is.na(cpp_out$n_grp), ]
  r_out[is.na(r_out$n_grp), ]

  m = merge(cpp_out, r_out, all = T)
  m[is.na(n_grp.x)]
  m[is.na(n_grp.y)]

  expect_equal(m$n_grp.x, m$n_grp.y)
  expect_equal(m$s_grp.x, m$s_grp.y)
})
