`%>%` <- magrittr::`%>%`

# Create summary table of design effects
create_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}

# Create design matrix for model of hierarchical coefficients
create_l2_design_matrix <- function(.XX, .arg) {
  if (is.null(.arg$level2_modifiers)) {
    zz.names <- list(.arg$use_t, dimnames(.XX)[[2]], "Zero")
    ZZ <- array(data = 0, dim = lapply(zz.names, length),
      dimnames = zz.names)
  } else {
    assertthat::not_empty(.arg$level2)
    assertthat::assert_that(is_subset(.arg$level2_modifiers, names(.arg$level2)))
    level2 <- .arg$level2 %>% dplyr::select_(.arg$time_id, .arg$geo_id, .arg$level2_modifiers)
    ZZ <- suppressWarnings(reshape2::melt(level2, id.vars = c(.arg$time_id, .arg$geo_id)))
    assertthat::assert_that(is.numeric(ZZ$value))
    ZZ <- suppressWarnings(reshape2::acast(ZZ, formula(paste(.arg$time_id,
            .arg$geo_id, "variable", sep = " ~ "))))
  }
  assertthat::assert_that(all(notNA(ZZ)))
  return(ZZ)
}

# Create 'greater than' indicators
create_gt_variables <- function(d, .items){
  out <- lapply(.items, function(item) {
    if (is.ordered(d[[item]])) {
      item_levels <- na.omit(levels(droplevels(d[[item]])))
      values <- match(as.character(d[[item]]), item_levels)
    } else if (is.numeric(d[[item]])) {
      item_levels <- sort(na.omit(unique(d[[item]])))
      values <- match(d[[item]], item_levels)
    } else {
      stop("each item should be an ordered factor or numeric")
    }
    message("'", item, "' is class '", class(d[[item]]), "' with ", length(item_levels),
      " non-missing values: '", paste(item_levels, collapse = "', '"), "'")
    gt_levels <- seq_along(item_levels)[-length(item_levels)]
    if (length(gt_levels) < 1) stop("no variation in item ", deparse(item))
    if (identical(length(gt_levels), 1L)) {
      assertthat::assert_that(identical(length(item_levels), 2L))
      message("\t considered binary with failure='", item_levels[1], "' and success='", item_levels[2], "'")
    }
    if (length(gt_levels) > 1L) {
      assertthat::assert_that(length(item_levels) > 2L)
      message("\t considered ordinal with levels '", paste(item_levels, collapse = "', '"), "' (ascending)")
    }
    gt_cols <- lapply(gt_levels, function(gt) {
      ifelse(values > gt, 1L, 0L)
    })
    assertthat::assert_that(assertthat::not_empty(gt_cols))
    gt_names <- paste(item, gt_levels, sep = "_gt")
    setNames(gt_cols, gt_names)
  })
  dplyr::bind_cols(out)
}

# Replace NA in a vector with 0
replaceNA <- function(x) {
  replace(x, is.na(x), 0)
}

# Replace NaN in a vector with NA
replaceNaN <- function(x) {
    replace(x, is.nan(x), NA)
}

# Get count of non-NA values over x
countValid <- function(x) {
    sum(!is.na(x))
}

anyValid <- function(x) {
  any(!is.na(x))
}

# Check element-wise if !is.na over x
notNA <- function(x) {
    !is.na(x)
}
