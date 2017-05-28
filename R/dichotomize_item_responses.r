dichotomize <- function(item_data, ctrl) {
  # Dichotomize item response variables
  #
  # For item response variables with K ordered levels, make K - 1 indicators for
  # whether a response is ranked higher than k.

  # Get e.g. names A,B,C, where B,C are item variables
  original_names <- data.table::copy(names(item_data))
  # After coercion we might have variables A,B,D,E, where B,D,E are items
  item_data <- coerce_item_types(item_data, ctrl)
  # Keep in item_names those that remain after type coercion, say B
  ctrl@item_names <- intersect(ctrl@item_names, names(item_data))
  # Add those created during type coercion: B,ABDE-ABC=DE
  ctrl@item_names <- c(ctrl@item_names, setdiff(names(item_data),
      original_names))
  stopifnot(length(ctrl@item_names) > 0)
  stopifnot(identical(unique(ctrl@item_names), ctrl@item_names))

  for (k in ctrl@item_names) {
    stopifnot(is.numeric(item_data[[k]]))
    gt_cols <- dichotomize_r(item_data[[k]])
    cn <- paste0(k, sub("^X", "", names(gt_cols)))
    item_data[, c(cn) := gt_cols]
    data.table::setattr(item_data, "gt_items", c(attr(item_data, "gt_items"),
                                                 cn))
  }
  invisible(item_data)
}

dichotomize_r <- function(vec) {
  vec <- as.numeric(vec)
  uniques <- sort(unique(na.omit(vec)))
  ret <- data.table::data.table("X_gt1" = rep(NA, length(vec)))
  for (i in seq.int(0, max(0, length(uniques) - 2))) {
    ret[, paste0("X_gt", i + 1) := as.integer(vec > uniques[i + 1])]
  }
  ret[]
}

coerce_item_types <- function(item_data, ctrl) {
  # Coerce item response variables to numeric
  # 
  # Check item variable types. If logical or ordered, coerce them to numeric
  # without loss of information. If character or factor variables with only 2
  # levels, do the same; otherwise, one-hot encode them with a warning.
  for (k in ctrl@item_names) {
    k_levels <- unique(na.omit(item_data[[k]]))
    if (length(k_levels) == 1) {
      # k doesn't vary (this should be caught earlier)
      stop("'", k, "' doesn't vary after restricting the data; it cannot ",
        "be used as an item variable.")
    }
    if (inherits(item_data[[k]], c("logical", "ordered"))) {
      # We can coerce k to numeric without losing information. If it has more
      # than 2 levels, dichotomize() will later dichotomize it.
      coerced <- as.numeric(item_data[[k]])
      item_data[, (k) := coerced]
    } else if (inherits(item_data[[k]], c("character", "factor"))) {
      # The levels of k have ambiguous rank. One-hot encode them.
      if (length(k_levels) > 2) {
        # one-hot encoding could discard information
        warning("The ", length(k_levels), " values of ",
          class(item_data[[k]]), " variable '", k, "' will be encoded as ",
          length(k_levels) - 1, " indicators. If the values of '", k,
          "' have rank order, coerce it to numeric or ordered to retain this ",
          "information.")
      }
      # one-hot encode k as nlevels-1 indicators
      for (i in seq.int(length(k_levels) - 1)) {
        item_data[, paste0(k, "_", i) := as.numeric(get(k) == k_levels[i])]
      }
      item_data[, (k) := NULL]
    }
  }
  return(item_data)
}

