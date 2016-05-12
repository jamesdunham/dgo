dichotomize <- function(item_data, ctrl) {
  # Dichotomize item response variables
  #
  # For item response variables with K ordered levels, make K - 1 indicators for
  # whether a response is ranked higher than k.
  #
  # Joining the result to the existing item_data with cbind is slow, but there
  # were problems in earlier implementations adding the dichotomized variables
  # as new columns by reference.
  gt_table <- as.data.frame(lapply(ctrl@item_names, function(i) {
    values <- item_data[[i]]
    gt_cols <- is_greater(values)
    setNames(gt_cols, paste0(i, sub("^X", "", names(gt_cols))))
  }))
  item_data <- cbind(item_data, gt_table)
  setattr(item_data, "gt_names", names(gt_table))
  invisible(item_data)
}

create_gt_variables <- function(item_data, ctrl) {
}
