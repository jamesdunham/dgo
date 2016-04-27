restrict_items <- function(item_data, ctrl) {
  data.table::setDT(item_data)
  extra_colnames <- setdiff(names(item_data),
                            c(ctrl@item_names,
                              ctrl@strata_names,
                              ctrl@survey_name,
                              ctrl@geo_name,
                              ctrl@time_name,
                              ctrl@group_names,
                              ctrl@weight_name))
  if (length(extra_colnames)) {
    item_data[, c(extra_colnames) := NULL]
  }
  coerce_factors(item_data, c(ctrl@group_names, ctrl@geo_name,
                              ctrl@survey_name))
  rename_numerics(item_data, c(ctrl@group_names, ctrl@geo_name,
                               ctrl@survey_name))
  initial_dim <- dim(item_data)
  final_dim <- c()
  iter <- 1L
  while (!identical(initial_dim, final_dim)) {
    message("Applying restrictions, pass ", iter, "...")
    if (identical(iter, 1L)) item_data <-
      drop_rows_missing_covariates(item_data, ctrl)
    initial_dim <- dim(item_data)
    item_data <- keep_t(item_data, ctrl)
    item_data <- keep_geo(item_data, ctrl)
    drop_responseless_items(item_data, ctrl)
    drop_items_rare_in_time(item_data, ctrl)
    drop_items_rare_in_polls(item_data, ctrl)
    item_data <- drop_itemless_respondents(item_data, ctrl)
    final_dim <- dim(item_data)
    iter <- iter + 1L
    if (identical(initial_dim, final_dim)) {
      message("\tNo changes")
    } 
  }
  setkeyv(item_data, c(ctrl@geo_name, ctrl@time_name))
  invisible(item_data)
}

restrict_modifier <- function(item_data, modifier_data, ctrl) {
  if (length(modifier_data)) {
    data.table::setDT(modifier_data)

    coerce_factors(modifier_data, c(ctrl@modifier_names,
                                    ctrl@t1_modifier_names,
                                    ctrl@geo_name,
                                    ctrl@time_name))

    modifier_data <- modifier_data[modifier_data[[ctrl@geo_name]] %chin%
                                   item_data[[ctrl@geo_name]]]
    if (!nrow(modifier_data))
      stop("no rows in modifier data remaining after subsetting to local ",
           "geographic areas in item data")
    modifier_data <- modifier_data[modifier_data[[ctrl@time_name]] %in%
                                   item_data[[ctrl@time_name]]]
    if (!nrow(modifier_data))
      stop("no rows in modifier data remaining after subsetting to time ",
           "periods in item data")

    n <- nrow(unique(modifier_data[, c(ctrl@geo_name, ctrl@time_name), with = FALSE]))
    if (!identical(nrow(modifier_data), n))
      stop("time and geo identifiers don't uniquely identify modifier data ",
           "observations")
   
    message("\nRestricted modifier data to time and geo observed in item data.")
  }
  invisible(modifier_data)
}

restrict_aggregates <- function(aggregate_data, ctrl) {
  if (length(aggregate_data)) {
    data.table::setDT(aggregate_data)

    coerce_factors(aggregate_data, c(ctrl@group_names, ctrl@geo_name,
                                     ctrl@time_name))

    aggregate_data <- aggregate_data[aggregate_data[[ctrl@geo_name]] %chin%
                                     ctrl@geo_filter]
    if (!nrow(aggregate_data))
      stop("no rows in aggregate data remaining after subsetting to local ",
           "geographic areas in `geo_filter`")
    aggregate_data <- aggregate_data[aggregate_data[[ctrl@time_name]] %in%
                                     ctrl@time_filter]
    if (!nrow(aggregate_data))
      stop("no rows in aggregate data remaining after subsetting to time ",
           "periods in `time_filter`")

    aggregate_data <- aggregate_data[aggregate_data[["item"]] %chin%
                                     ctrl@aggregate_item_names]
    if (!nrow(aggregate_data))
      stop("no rows in aggregate data remaining after subsetting to items ",
           "in `aggregate_item_names`")

    aggregate_data <- aggregate_data[get("n_grp") > 0]
    if (!nrow(aggregate_data))
      stop("no rows in aggregate data remaining after dropping unobserved ",
           "group-item combinations")

    extra_colnames <- setdiff(names(aggregate_data),
                              c(ctrl@geo_name, ctrl@time_name, ctrl@group_names, "item", "s_grp", "n_grp"))
    if (length(extra_colnames)) {
      aggregate_data[, c(extra_colnames) := NULL, with = FALSE]
    }
    aggregate_data
  }
}

coerce_factors <- function(tbl, vars) {
  factor_vars <- vars[vapply(tbl[, vars, with = FALSE], is.factor, logical(1))]
  if (length(factor_vars)) {
    for (v in factor_vars) {
      warning("Coercing factor `", v, "` in ", substitute(tbl), 
              " with `as.character(", substitute(tbl), "[[", v, "]])`")
      tbl[, c(v) := as.character(tbl[[v]])]
    }
  }
  invisible(tbl)
}

rename_numerics <- function(tbl, vars) {
  numeric_vars <- vars[vapply(tbl[, vars], is.numeric, logical(1))]
  if (length(numeric_vars)) {
    for (v in numeric_vars) {
      warning("coercing numeric `", v, "` in ", substitute(tbl),
              " with `paste0(", v, ", ", substitute(tbl), "[[", v, "]])`")
      tbl[, c(v) := paste0(v, tbl[[v]])]
    }
  }
  invisible(tbl)
}

drop_rows_missing_covariates <- function(item_data, ctrl) {
  n <- nrow(item_data)
  is_missing <- rowSums(is.na(item_data[, c(ctrl@geo_name, ctrl@time_name, ctrl@group_names, ctrl@survey_name), with = FALSE])) > 0
  item_data <- subset(item_data, !is_missing)
  if (!identical(n, nrow(item_data))) {
    message("\tDropped ", format(n - nrow(item_data), big.mark = ","),
            " rows for missingness in covariates")
  }
  item_data
}

keep_t <- function(item_data, ctrl) {
  item_data <- item_data[get(ctrl@time_name) %in% ctrl@time_filter]
  invisible(item_data)
}

keep_geo <- function(item_data, ctrl) {
  item_data <- item_data[get(ctrl@geo_name) %chin% ctrl@geo_filter]
  invisible(item_data)
}

drop_responseless_items <- function(item_data, ctrl) {
  item_names <- intersect(ctrl@item_names, names(item_data))
  response_n <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) == 0),
                          .SDcols = item_names]
  response_n <- item_data[, lapply(.SD, function(x) sum(!is.na(x))),
                          .SDcols = item_names]
  response_n <- melt.data.table(response_n, id.vars = NULL,
                                measure.vars = names(response_n),
                                variable.name = "variable",
                                value.name = "count") 
  responseless_items <- as.character(response_n[get("count") == 0][["variable"]])
  if (length(responseless_items)) {
    item_data[, c(responseless_items) := NULL]
    message(sprintf(ngettext(length(responseless_items),
          "\tDropped %i item for lack of responses",
          "\tDropped %i items for lack of responses"),
        length(responseless_items)))
    if (!length(intersect(ctrl@item_names, names(item_data))))
      stop("no items remaining after dropping items without responses")
  }
  invisible(item_data)
}

drop_itemless_respondents <- function(item_data, ctrl) {
  item_names <- intersect(ctrl@item_names, names(item_data))
  if (!length(item_names)) stop("no items remaining")
  if (!nrow(item_data)) stop("no rows remaining")
  item_data[, c("n_responses") := list(rowSums(!is.na(.SD)) == 0L),
            .SDcols = item_names]
  n_itemless <- sum(item_data[["n_responses"]])
  if (n_itemless > 0) {
    item_data <- item_data[!get("n_responses")]
    message(sprintf(ngettext(n_itemless,
          "\tDropped %i row for lacking item responses",
          "\tDropped %i rows for lacking item responses"),
        n_itemless))
    if (!nrow(item_data))
      stop("no rows remaining after dropping rows without item responses")
  }
  invisible(item_data)
}

drop_items_rare_in_time <- function(item_data, ctrl) {
  item_names <- intersect(ctrl@item_names, names(item_data))
  if (!length(item_names)) stop("no items remaining")
  if (!nrow(item_data)) stop("no rows remaining")
  setkeyv(item_data, item_data[, ctrl@time_name])
  response_t <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) > 0), .SDcols
                          = item_names, by = eval(item_data[, ctrl@time_name])]
  response_t <- melt.data.table(response_t, id.vars = ctrl@time_name,
                                variable.name = "variable",
                                value.name = "observed")
  response_t <- response_t[, list(count = sum(get("observed"))), keyby = "variable"]
  response_t <- response_t[get("count") < ctrl@min_t_filter]
  rare_items <- as.character(response_t[["variable"]])
  if (length(rare_items)) {
    for (v in rare_items) {
      item_data[, c(v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_t requirement (%i)",
          "\tDropped %i items for failing min_t requirement (%i)"),
        length(rare_items), ctrl@min_t_filter))
    if (!length(intersect(ctrl@item_names, names(item_data))))
      stop("no items remaining after dropping items without responses")
  }
  invisible(item_data)
}

drop_items_rare_in_polls <- function(item_data, ctrl) {
  item_names <- intersect(ctrl@item_names, names(item_data))
  if (!length(item_names)) stop("no items remaining")
  if (!nrow(item_data)) stop("no rows remaining")
  #TODO: dedupe; cf. drop_items_rare_in_time
  setkeyv(item_data, item_data[, ctrl@survey_name])
  item_survey <- item_data[, lapply(.SD, function(x) sum(!is.na(x)) > 0),
                           .SDcols = item_names,
                           by = eval(item_data[, ctrl@survey_name])]
  item_survey <- melt.data.table(item_survey, id.vars =
                                 ctrl@survey_name)[get("value")]
  item_survey <- item_survey[, c("N") := .N, by = "variable"]
  item_survey <- item_survey[get("N") < ctrl@min_survey_filter]
  rare_items <- as.character(item_survey[["variable"]])
  if (length(rare_items)) {
    for (v in rare_items) {
      item_data[, c(v) := NULL]
    }
    message(sprintf(ngettext(length(rare_items),
          "\tDropped %i items for failing min_survey requirement (%i)",
          "\tDropped %i items for failing min_survey requirement (%i)"),
        length(rare_items), ctrl@min_survey_filter))
    if (!length(intersect(ctrl@item_names, names(item_data))))
      stop("no items remaining after dropping items without responses")
  }
  invisible(item_data)
}

get_observed <- function(item_data, aggregate_data, varname) {
  unique(item_data[[varname]], aggregate_data[[varname]])
}
