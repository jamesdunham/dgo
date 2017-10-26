suppressMessages(library(data.table))

minimal_individual_data = data.table::data.table(
  period = rep(1:2, each = 5),
  geo = c(rep('A', 2), rep('B', 3)),
  pid = rep(c('D', 'R'), 5),
  item_1 = c(rep(0, 2), rep(1, 3)),
  item_2 = c(0, rep(1, 4))
)

minimal_aggregate_data = data.table::data.table(
  expand.grid(pid = c('D', 'R'),
    geo = c('A', 'B'),
    period = 1:2,
    item = c('item_1', 'item_2'), stringsAsFactors = FALSE),
  s_grp = 0:1,
  n_grp = 2
)

extract_counts = function(dgirt_in) {
  counts = data.table::data.table(
    s_vec = dgirt_in$s_vec,
    n_vec = dgirt_in$n_vec,
    count_id = names(dgirt_in$n_vec)
    )
  counts[, c('period', 'geo', 'pid', 'item') := tstrsplit(count_id, '__')]
  counts[grepl('_gt\\d+$', item), item := substr(item, 1, nchar(item) - 4)]
  counts[, period := as.integer(period)]
  counts[, count_id := NULL]
  setorderv(counts, c('period', 'geo', 'pid', 'item'))
  setcolorder(counts, c('period', 'geo', 'pid', 'item', 's_vec', 'n_vec'))
  counts[]
}

get_manual_count = function(.data, group_vars = c('period', 'geo', 'pid')) {
  long = melt(.data,
    id.vars = group_vars,
    variable.name = 'item',
    variable.factor = FALSE)
  setorderv(long, c(group_vars, 'item'))
  counts = long[, .(s_vec = sum(value), n_vec = .N), by = c(group_vars)]
  setcolorder(counts, c('period', 'geo', 'pid', 'item', 's_vec', 'n_vec'))
  counts[]
}

minimal_individual_result = suppressMessages(shape(
  item_data = minimal_individual_data,
  item_names = 'item_1',
  time_name = 'period',
  geo_name = 'geo',
  group_names = 'pid'))

minimal_indiv_agg_result = suppressMessages(shape(
  item_data = minimal_individual_data,
  item_names = 'item_1',
  aggregate_data = minimal_aggregate_data,
  # FIXME: stringsAsFactors = TRUE in expand.grid above leads to error
  time_name = 'period',
  geo_name = 'geo',
  group_names = 'pid'))

minimal_2_item_individual_result = suppressMessages(shape(
  item_data = minimal_individual_data,
  item_names = c('item_1', 'item_2'),
  time_name = 'period',
  geo_name = 'geo',
  group_names = 'pid'))

min_item_call <- function(...) {
  default <- list(item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_groupless_call <- function(...) {
  default <- list(item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_modifier_call <- function(...) {
  default <- list(item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female",
                  modifier_data = states,
                  modifier_names = "prop_evangelicals",
                  t1_modifier_names = "prop_evangelicals")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

min_agg_call <- function(...) {
  default <- list(aggregate_data = aggregates,
                  item_data = opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "female")
  dots <- list(...)
  dots <- c(dots, default[!names(default) %in% names(dots)])
  invisible(do.call(shape, dots))
}

