`%>%` <- magrittr::`%>%`

# Create summary table of design effects
create_design_effects <- function(x) {
    stopifnot(is.numeric(x))
    y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
    if (is.na(y))
        return(1) else return(y)
}

# Create design matrix for model of hierarchical coefficients
create_l2_design_matrix <- function(.XX, .arg) {
  # .XX = XX
  # .arg = arg
  if (is.null(.arg$level2_modifiers)) {
    zz.names <- list(.arg$use_t, dimnames(.XX)[[2]], "Zero")
    ZZ <- array(data = 0, dim = lapply(zz.names, length),
      dimnames = zz.names)
  } else {
    stopifnot(!is.null(.arg$level2))
    # stopifnot(all(c(.arg$time_id, .arg$geo_id) %in% names(.arg$level2)))
    stopifnot(all(.arg$level2_modifiers %in% names(.arg$level2)))
    level2 = .arg$level2 %>% dplyr::select_(.arg$time_id, .arg$geo_id, .arg$level2_modifiers)
    ZZ <- suppressWarnings(reshape2::melt(level2, id.vars = c(.arg$time_id, .arg$geo_id)))
    stopifnot(inherits(ZZ$value, "numeric"))
    ZZ <- suppressWarnings(reshape2::acast(ZZ, formula(paste(.arg$time_id,
            .arg$geo_id, "variable", sep = " ~ "))))
  }
  if (any(is.na(ZZ))) {
    stop("No cell of ZZ should be NA")
  }
  return(ZZ)
}

# Create 'greater than' indicators
create_gt_variables <- function(d, .items) {
    for (q in .items) {
        .levels <- levels(factor(d[[q]]))
        .levels <- .levels[-length(.levels)]
        varname <- paste0(q, "_gt", .levels)
        d[, varname] <- sapply(.levels, function(l) {
            as.numeric(d[[q]] > as.numeric(l))
        })
    }
    return(d)
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

# Check if any !is.na over x
anyValid <- function(x) {
  any(!is.na(x))
}

# Check element-wise if !is.na over x
notNA <- function(x) {
    !is.na(x)
}
