#' @export
shape_modgirt = function(data, items, time, geo, groups = NULL, weight = NULL) {

  opin_long <- data %>%
    # Keep only specified variables
    dplyr::select_at(dplyr::vars(dplyr::one_of(items, time, geo,
          groups, weight))) %>%
    # While data are still wide, assign respondent IDs
    dplyr::mutate(respondent_id = dplyr::row_number()) %>%
    # Melt to long
    reshape2::melt(measure.vars = items, variable.name = "item")

  # TODO: Create weights if they don't exist 
  if (!length(weight)) {
    weight <- 'weight'
    opin_long$weight = 1
    # ...
  }

  # Create time variable if it doesn't exist
  if (!length(time)) {
    time <- 'time'
    opin_long$time = 1
  }
  
  # Filter rows such that no values of `value`, `weight`, `geo`, or
  # `groups` will be NA 
  opin_long <- opin_long %>%
    dplyr::filter_at(dplyr::vars(dplyr::one_of("value", weight, geo,
          groups)),
      dplyr::all_vars(!is.na(.)))

   # TODO: catch cast failures
  opin_long <- opin_long %>%
    dplyr::mutate(level = as.integer(value)) %>%
    dplyr::arrange(respondent_id, item)

  # The result is a dataframe in which each row represents the item response of a
  # survey respondent. 

  tgq <- c(time, geo, groups, "item")

  unused_cut = unobserved_levels(opin_long)

  # 1. Count items answered by respondent (denoted r_i)
  opin_long <- opin_long %>%
    dplyr::group_by(respondent_id) %>%
    dplyr::mutate(n_responses = dplyr::n())

  # 2. Compute adjusted respondent weights (denoted w^*)
  opin_long <- opin_long %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(tgq))) %>%
    # FIXME: use weight
    dplyr::mutate(weight_star = weight / mean(weight))
  stopifnot(!any(is.na(opin_long$weight_star)))

  # 3. Compute group design effects (denoted d_{tgq})
  opin_long <- opin_long %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(tgq))) %>%
    dplyr::mutate(design_effect = calc_def(weight_star))
  assert(!any(is.na(opin_long$design_effect)))

  # 4. Compute n*
  n_star <- opin_long %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(tgq))) %>%
    dplyr::summarise(n = n(),
      n_star = sum(1 / (n_responses * design_effect)))

  # 5a. Start calculating s*
  s_star <- opin_long %>%
    # denominator is the (unweighted) observation count in t,g,q
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(tgq))) %>%
    dplyr::mutate(n_tgq = dplyr::n()) %>%
    # numerator is the observation count in t,g,q,k multipled by w*
    dplyr::group_by(level, add = TRUE) %>%
    # This gives us a fraction of n*
    dplyr::summarise(s_prop = sum(weight_star / n_tgq))

  # 5b. Join s*-in-progress and n*
  ns_star <- s_star %>%
    dplyr::left_join(y = n_star, by = c(geo, groups, time, 'item'))
  assert(nrow(ns_star) == nrow(s_star))
  assert(!assertthat::has_name(ns_star, 's_star'))

  # 5c. Finish calculating s*
  ns_star <- ns_star %>%
    dplyr::mutate(s_star = n_star * s_prop) %>%
    dplyr::ungroup()

  # 6. Create group variable
  assert(!assertthat::has_name(ns_star, 'group'))
  ns_star$group <- do.call(interaction,
    list(ns_star[c(geo, groups)], sep = " | "))

  # Create a 4-dimensional array of responses indexed time, group, item, and level
  cast_formula <- make_formula(list(time, "group", "item", "level"))
  SSSS_ord <- ns_star %>%
    dplyr::ungroup() %>%
    reshape2::acast(cast_formula, fun.aggregate = sum, value.var = "s_star",
      drop = FALSE)

  # dimnames(SSSS_ord)
  # apply(SSSS_ord, 4, function (x) sum(x > 0))

  stan_data = list(
    T = dim(SSSS_ord)[1],
    G = dim(SSSS_ord)[2],
    Q = dim(SSSS_ord)[3],
    K = dim(SSSS_ord)[4],
    D = 1,
    SSSS = SSSS_ord,
    beta_sign = matrix(1, dim(SSSS_ord)[3], 1),
    unused_cut = unused_cut,
    N_nonzero = sum(SSSS_ord > 0))

  shaped = new('modgirt_in',
    items = items,
    time = time,
    geo = geo,
    demo = groups,
    stan_data = stan_data)

  shaped
}

unobserved_levels = function(responses_long) {
  # Identify categories with no responses in `unused_cut`, a Q x K-1 matrix
  assert(assertthat::has_name(responses_long, 'item'))
  assert(assertthat::has_name(responses_long, 'value'))
  item_max <- responses_long %>%
    dplyr::group_by(item) %>%
    dplyr::summarise(max_level = max(value))
  unobserved <- matrix(1,
    nrow = nrow(item_max),  # a row for each item
    ncol = max(item_max$max_level) - 1,  # a column for each level (across all items)
    dimnames = list(item_max$item, NULL))
  for (i in 1:nrow(unobserved)) {
    unobserved[i, 1:(item_max$max_level[i]) - 1] <- 0
  }
  unobserved
}

calc_def <- function (w) {
  if (length(w) > 1) {
    1 + sd(w) / mean(w)
  } else {
    1
  }
}

# FIXME
make_formula <- function(var_list) {
  form_char_vec <- vector("character", length(var_list))
  for (l in seq_along(var_list)) {
    form_char_vec[l] <- paste(var_list[[l]], collapse = " + ")
  }
  form_char <- do.call(paste, list(form_char_vec, collapse = " ~ "))
  as.formula(form_char)    
}


drop_row_if_any_na = function(.data, varnames) {
  .data[!rowSums(is.na(.data[, c(varnames), with = FALSE])) > 0][]
}

drop_row_if_all_na = function(.data, varnames) {
  .data[rowSums(is.na(.data[, c(varnames), with = FALSE])) != length(varnames)][]
}

all_na_cols = function(.data, varnames) {
  all_na = .data[, lapply(.SD, function(x) all(is.na(x))), .SDcols = varnames]
  varnames[unlist(all_na)]
}

