shape_hierarchical_data <- function(modifier_data, modifier_names, group_grid_t,
  XX, ctrl) {
  # The array of hierarchical data ZZ should be T x P x H, where T is the
  # number of time periods, P is the number of hierarchical parameters
  # (including the geographic), and H is the number of predictors for
  # geographic unit effects.
  if (!length(modifier_names)) {
   return(zero_zz(XX, ctrl))
  } 
  hierarchical <- data.table::copy(modifier_data)
  hierarchical <- drop_extra_cols(hierarchical, modifier_names, ctrl) 
  data.table::setkeyv(hierarchical, c(ctrl@geo_name, ctrl@time_name))
  if (length(ctrl@group_names)) {
    unmodeled <- zero_unmodeled(hierarchical, modifier_names, group_grid_t, ctrl) 
    hierarchical <- rbind(hierarchical, unmodeled)
  }
  zz <- create_zz(hierarchical, modifier_names, ctrl)
  return(zz)
}

zero_zz <- function(XX, ctrl) {
  zz_dimnames <- list(ctrl@time_filter, dimnames(XX)[[2]], "")
  zz <- array(data = 0, dim = lapply(zz_dimnames, length), dimnames =
    zz_dimnames)
  zz
}

drop_extra_cols <- function(hierarchical, modifier_names, ctrl) {
  extra_colnames <- setdiff(names(hierarchical), c(ctrl@geo_name,
      ctrl@time_name, modifier_names))
  if (length(extra_colnames)) {
    hierarchical[, c(extra_colnames) := NULL]
  }
  hierarchical
}

zero_unmodeled <- function(hierarchical, modifier_names, group_grid_t, ctrl) {
  # make a zeroed table for unmodeled parameters by time period
  modeled_param_names <- unique(hierarchical[[ctrl@geo_name]])
  # unmodeled param levels will be those of groups
  unmodeled_param_levels = unlist(lapply(ctrl@group_names, function(x) {
      paste0(x, unique(group_grid_t[[x]]))[-1]
    }))
  unmodeled_frame <- expand.grid(c(list(unmodeled_param_levels,
        ctrl@time_filter), rep(list(0L), length(modifier_names))))
  unmodeled_frame <- setNames(unmodeled_frame, c(ctrl@geo_name, ctrl@time_name,
      modifier_names))
  data.table::setDT(unmodeled_frame, key = c(ctrl@geo_name, ctrl@time_name))
  unmodeled_frame
}

create_zz <- function(hierarchical, modifier_names, ctrl) {
  unique_time <- unique(hierarchical[[ctrl@time_name]])
  unique_geo <- unique(hierarchical[[ctrl@geo_name]])
  zz <- sapply(modifier_names, function(x) {
    matrix(hierarchical[[x]],
      # We have T rows, so filling by column is correct SO LONG AS time varies
      # fastest then geo in hierarchical
      nrow = length(unique_time), ncol = length(unique_geo),
      dimnames = list(unique_time, unique_geo))
      }, simplify = 'array')
  # omit first geo parameter
  zz <- zz[, -1, , drop = FALSE]
  zz
}

