dichotomize <- function(item_data, ctrl) {
  # Dichotomize item response variables
  #
  # For item response variables with K ordered levels, make K - 1 indicators for
  # whether a response is ranked higher than k.
  for (i in ctrl@item_names) {
    stopifnot(is.numeric(item_data[[i]]))
    gt_cols <- dichotomize_cpp(item_data[[i]])
    cn <- paste0(i, sub("^X", "", names(gt_cols)))
    item_data[, c(cn) := gt_cols]
    data.table::setattr(item_data, "gt_items",
                        c(attr(item_data, "gt_items"), cn))
  }
  invisible(item_data)
}
