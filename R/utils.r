`%>%` <- magrittr::`%>%`

# Create summary table of design effects
create_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  if (is.na(y)) {
    return(1)
  } else {
    return(y)
  }
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
  out <- lapply(.items, function(stub) {
    if (is.factor(d[[stub]])) {
      item_levels <- levels(d[[stub]])
      values <- as.numeric(d[[stub]])
    } else {
      item_levels <- as.character(sort(unique(d[[stub]])))
      values <- d[[stub]]
    }
    gt_levels <- item_levels[-length(item_levels)]
    gt_names <- paste(stub, gt_levels, sep = "_gt")
    gt_cols <- lapply(gt_levels, function(gt) {
      values > as.numeric(gt)
    })
    assertthat::assert_that(assertthat::not_empty(gt_cols))
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
