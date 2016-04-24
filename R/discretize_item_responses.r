discretize <- function(item_data, ctrl) {
  # Discretize item response variables
  #
  # For item response variables with K ordered levels, make K - 1 indicators for
  # whether a response is ranked higher than k.
  gt_table <- create_gt_variables(item_data, ctrl)
  # NOTE: updating item_data by reference to include gt_table
  item_data[, names(gt_table) := gt_table]
  # return the indicator names to reference them safely later
  names(gt_table)
}

create_gt_variables <- function(item_data, ctrl){
  widths <- c("item" = 30, "class" = 10, "levels" = 12, "responses" = 16)
  print_varinfo_header(widths)
  out <- lapply(ctrl@item_names, function(i) {
    if (is.ordered(item_data[[i]])) {
      i_levels <- na.omit(levels(droplevels(item_data[[i]])))
      values <- match(as.character(item_data[[i]]), i_levels)
    } else if (is.numeric(item_data[[i]])) {
      i_levels <- sort.int(na.omit(unique.default(item_data[[i]])))
      values <- match(item_data[[i]], i_levels)
    } else {
      stop("each item should be an ordered factor or numeric")
    }
    gt_levels <- seq_along(i_levels)[-length(i_levels)]

    if (!length(gt_levels)) {
      stop("no variation in item ", deparse(i))
    } else if (identical(length(gt_levels), 1L)) {
      stopifnot(identical(length(i_levels), 2L))
    } else if (length(gt_levels) > 1L) {
      stopifnot(length(i_levels) > 2L)
    }

    print_varinfo(item_data, i, i_levels, gt_levels, widths)
    gt_cols <- lapply(gt_levels, function(gt) {
      is_greater(values, gt)
    })
    assertthat::assert_that(not_empty(gt_cols))
    setNames(gt_cols, paste(i, gt_levels, sep = "_gt"))
  })
  print_varinfo_rule(widths)
  data.frame(out)
}

print_varinfo_header <- function(widths) {
  pad_side <- c("right", rep("left", length(widths) - 1))
  message()
  message(concat_varinfo(widths, names(widths), pad_side))
  print_varinfo_rule(widths)
}

print_varinfo_rule <- function(widths) {
  message(paste0(rep('-', sum(widths)), collapse = ''))
}

print_varinfo <- function(item_data, i, i_levels, gt_levels, widths) {
  item_name = ifelse(nchar(i) > 30,paste0(stringi::stri_sub(i, 1, 26), '...'), i)
  responses = format(sum(!is.na(item_data[[i]])), big.mark = ",")
  values <- c("item" = item_name, "class" = class(item_data[[i]]), "levels" = length(i_levels), responses = responses)
  pad_side <- c("right", rep("left", length(values) - 1))
  assertthat::assert_that(identical(length(widths), length(values)))
  message(concat_varinfo(widths, values, pad_side))
}

concat_varinfo = function(widths, values, pad_side) {
  mapply(function(width, value, side) {
      paste0(stringi::stri_pad(value, width, side = side), collapse = '')
    }, widths, values, pad_side)
}
