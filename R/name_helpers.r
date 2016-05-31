flatnames <- function(dgirt_out, fnames = NULL) {

  control <- dgirt_out@dgirt_in$control
  if (!length(fnames)) {
    fnames <- dgirt_out@sim$fnames_oi
  }

  fname_len <- length(fnames)
  ftab <- data.table::setDT(list(fname = fnames))
  ftab[, param := sub("(^.*)\\[.*", "\\1", fname)]
  i_match = regexpr("(?<=\\[)\\d+(?=\\]|,)", fnames, perl = TRUE)
  j_match = regexpr("(?<=,|\\[)\\d+(?=\\])", fnames, perl = TRUE)
  match_end <- function(x) x + attr(x, "match.length") - 1L
  ftab[, `:=`(i_start = i_match,
              i_end = match_end(i_match),
              j_start = j_match,
              j_end = match_end(j_match))]
  ftab[i_start > 0, i := type.convert(substr(fname, i_start, i_end))]
  ftab[j_start > 0, j := type.convert(substr(fname, j_start, j_end))]
  ftab[, grep("_start|_end", names(ftab)) := NULL]

  for (parname in unique(ftab[["param"]])) {
    # index_names is a list for looking up the names of parameter indexes
    for (i in index_names[[parname]]) {
      if (length(i)) {
        pos <- c("i", "j")[which(index_names[[parname]] == i)]
        ftab[param == parname, c(i) := get(pos), with = FALSE]
      }
    }
  }

  if ("group_names" %in% names(ftab)) {
    group_grid <- data.table::setDT(dgirt_out@dgirt_in$group_grid_t)
    group_grid[, group_names := .I]
    ftab <- merge(ftab, group_grid, by = "group_names", all.x = TRUE)
  }

  if ("time_name" %in% names(ftab)) {
    time_grid <- data.table::setDT(setNames(list(seq_along(control@time_filter),
                                                 control@time_filter),
                                            c("time_name", control@time_name)))
    ftab <- merge(ftab, time_grid, by = "time_name", all.x = TRUE)
  }

  if ("hier_params" %in% names(ftab)) {
    hier_grid <- data.table::setDT(list(hier_params = seq_along(dgirt_out@dgirt_in$hier_names),
                                        hier_param = dgirt_out@dgirt_in$hier_names))
    ftab <- merge(ftab, hier_grid, by = "hier_params", all.x = TRUE)
  }

  if ("item_names" %in% names(ftab)) {
    item_grid <- data.table::setDT(list(item_names = seq_along(dgirt_out@dgirt_in$gt_items),
                                        item = dgirt_out@dgirt_in$gt_items))
    ftab <- merge(ftab, item_grid, by = "item_names", all.x = TRUE)
  }

  drop_cols <- intersect(names(ftab), c("item_names", "hier_params",
                                        "time_name", "group_names", "i", "j"))
  if (length(drop_cols))
    ftab[, drop_cols := NULL, with = FALSE]

  # index_cols <- intersect(c(control@geo_name, control@group_names,
  #                           control@time_name, "hier_param", "item"),
  #                         names(ftab))
  # concat_cols <- function(SD) {
  #   SD <- lapply(SD, function(x) replace(x, is.na(x), ""))
  #   res <- do.call(paste, c(SD, sep = "__"))
  #   lapply(res, function(x) gsub("^__|__{2,}|__$", "", x))
  # }
  # ftab[, iname := concat_cols(.SD), .SDcols = index_cols]
  # ftab[, res := paste0(param, "[", iname, "]")]
  # ftab[, res := sub("\\[\\]", "", res)]

  stopifnot(identical(nrow(ftab), fname_len))
  ftab
}

arraynames <- function(dgirt_extract, dgirt_out) {

  control <- dgirt_out@dgirt_in$control

  dim2_indexed_t <- c('theta_bar', 'xi', 'gamma', 'delta_gamma', 'delta_tbar',
                      'nu_geo', 'sd_theta', 'sd_theta_bar', 'sd_total',
                      'theta_l2', 'var_theta_bar_l2')
  if (!as.logical(control@constant_item)) dim2_indexed_t <- c(dim2_indexed_t, "kappa")
  dim2_indexed_t <- intersect(dim2_indexed_t, names(dgirt_extract))

  for (i in dim2_indexed_t) {
    names(attributes(dgirt_extract[[i]])$dimnames)[2] <- 'time'
    stopifnot(identical(dim(dgirt_extract[[i]])[2], length(control@time_filter)))
    dimnames(dgirt_extract[[i]])[[2]] <- control@time_filter
  }

  if ('theta_bar' %chin% names(dgirt_extract)) {
    names(attributes(dgirt_extract[['theta_bar']])$dimnames)[3] <- 'group'
    groups_concat <- do.call(function(...) paste(..., sep = "__"), dgirt_out@dgirt_in$group_grid_t)
    stopifnot(identical(dim(dgirt_extract[['theta_bar']])[3], length(groups_concat)))
    dimnames(dgirt_extract[['theta_bar']])[[3]] <- groups_concat
  }

  if ('gamma' %chin% names(dgirt_extract)) {
    names(attributes(dgirt_extract[['gamma']])$dimnames)[3] <- 'param'
    assertthat::assert_that(identical(dim(dgirt_extract[['gamma']])[3], length(dgirt_out@dgirt_in$hier_names)))
    dimnames(dgirt_extract[['gamma']])[[3]] <- dgirt_out@dgirt_in$hier_names
  }

  if ('kappa' %chin% names(dgirt_extract)) {
    names(attributes(dgirt_extract[['kappa']])$dimnames)[3] <- 'item'
    assertthat::assert_that(identical(dim(dgirt_extract[['kappa']])[3], length(dgirt_out@dgirt_in$gt_items)))
    dimnames(dgirt_extract[['kappa']])[[3]] <- dgirt_out@dgirt_in$gt_items
  }

  if ('sd_item' %chin% names(dgirt_extract)) {
    names(attributes(dgirt_extract[['sd_item']])$dimnames)[2] <- 'item'
    assertthat::assert_that(identical(dim(dgirt_extract[['sd_item']])[2], length(dgirt_out@dgirt_in$gt_items)))
    dimnames(dgirt_extract[['sd_item']])[[2]] <- dgirt_out@dgirt_in$gt_items
  }

  if ('var_theta' %chin% names(dgirt_extract)) {
    names(attributes(dgirt_extract[['var_theta']])$dimnames)[2] <- 'time'
    assertthat::assert_that(identical(dim(dgirt_extract[['var_theta']])[2], length(control@time_filter)))
    dimnames(dgirt_extract[['var_theta']])[[2]] <- control@time_filter
  }

  dgirt_extract
}
