utils::globalVariables(c("fname", "group_names", "i_end", "i_start", "j",
                         "j_end", "j_start", "param"))

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
        ftab[param == parname, c(i) := get(pos)]
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
  if (length(drop_cols)) {
    ftab[, c(drop_cols) := NULL]
  }

  stopifnot(identical(nrow(ftab), fname_len))
  ftab
}
