#' Shape Model Data.
#'
#' `shape` prepares data for modeling with `dgirt`.
#'
#' `shape` takes four kinds of data. Only `item_data` is required. For each data
#' passed to `shape` through its `*_data` arguments, additional arguments are
#' required. These are described in the sections below. Most give the name or
#' names of key variables in the data; they end in `_name` or `_names` and
#' should be character vectors. A few implement preprocessing and modeling
#' choices.
#' 
#' @section Item Data:
#' These are required arguments if `item_data` is given.
#' \describe{
#'   \item{item_names}{Individual item responses. These variables should be
#'   integers or ordered factors in the data.}
#'   \item{group_names}{Discrete grouping variables, usually demographic. Using
#'   numeric variables is allowed but not recommended.}
#'   \item{geo_name}{A geographic variable representing local areas. In the
#'   data it should should be character or factor.}
#'   \item{time_name}{A time variable with numeric values.}
#'   \item{weight_name}{A variable giving survey weights.}
#' }
#'
#' @section Modifier Data:
#' These are required arguments if `modifier_data` is given.
#' \describe{
#'   \item{modifier_names}{Modifiers of geographic hierarchical parameters, e.g.
#'   median household income in each local-area and time-period combination.}
#'   \item{t1_modifier_names}{Modifiers to be used instead of those in
#'   `modifier_names` only in the first period.}
#' }
#'
#' @section Aggregate Data:
#' Specifying `aggregate_data` requires no additional arguments; instead, we
#' make many assumptions about the data. This implementation is likely to change
#' in the future. Variable names given for `item_data` are expected in the table
#' of aggregates: `item_names`, `group_names`, `geo_name`, and `time_name`. Two
#' fixed variable names are also expected in `aggregate_data`: `n_grp` giving
#' adjusted counts of item-response trials, and `s_grp` giving adjusted counts
#' of item-response successes. The counts should be adjusted consistently with
#' the transformations applied to the individual `item_data`.
#'
#' @section Preprocessing:
#' If `target_data` is specified `shape` will adjust the weighting of groups
#' toward population targets via raking. This relies on an adaptation of
#' `\link[survey]{rake}`. The additional required arguments are
#' `target_proportion_name` and `strata_names`. The implementation will be more
#' flexible in the future, but at the moment the strata are defined additively
#' when more than one variable is given in `strata_names`. 
#' 
#' `shape` can restrict data row-wise in `item_data`, `modifier_data`, and
#' `aggregate_data` to that within specified time periods (`time_filter`) and
#' local geographic areas (`geo_filter`). Data can also be filtered for
#' sparsity, to keep items that appear in a minimum of time periods or surveys.
#' This is a column-wise operation. If both row-wise and column-wise
#' restrictions are specified, `shape` iterates over them until they leave the
#' data unchanged.
#'
#' \describe{
#'   \item{target_proportion_name}{The variable giving population proportions
#'   for strata.}
#'   \item{strata_names}{Variables that define population strata.}
#'   \item{geo_filter}{A character vector giving values of the geographic
#'   variable. Defaults to observed values.}
#'   \item{time_filter}{A numeric vector giving possible values of the time
#'   variable. Observed and unobserved time periods can be given.}
#'   \item{min_survey_filter}{An integer minimum of survey appearances for
#'   included items. Defaults to 1 and requires `survey_name`.}
#'   \item{survey_name}{The variable that identifies surveys, for use with
#'   `min_survey_filter`.}
#'   \item{min_t_filter}{An integer minimum of time period appearances for
#'   included items.}
#' }
#'
#' @section Modeling Choices:
#' Optional. Most arguments like this one are in the `dgirt` signature, but
#' `constant_item` affects the shape of the data. May move to `dgirt` in the
#' future. 
#' \describe{
#'   \item{constant_item}{Whether item difficulty parameters should be constant
#'   over time. Default `TRUE`.}
#' }
#' @param item_data A table of individual item responses.
#' @param ... Further arguments. See details below.
#' @param modifier_data A table giving characteristics of `item_data`
#' identifiers to be modeled as hierarchical parameters.
#' @param target_data A table of population proportions.
#' @param aggregate_data A table of group-level item-response aggregates.
#' @return An object of the type expected by `\link{dgirt}`.
#' @import data.table
#' @examples
#' # model individual item responses 
#' data(state_opinion)
#' shaped_responses <- shape(state_opinion,
#'                           time_name = "year",
#'                           geo_name = "state",
#'                           item_names = "Q_cces2006_minimumwage",
#'                           group_names = "race",
#'                           survey_name = "source",
#'                           weight_name = "weight")
#' @export
shape <- function(item_data,
                  ...,
                  modifier_data = NULL,
                  target_data = NULL,
                  aggregate_data = NULL) {

  ctrl <- init_control(item_data, modifier_data, target_data, aggregate_data,
                       ...)
  d_in <- dgirtIn$new(item_data, modifier_data, target_data, aggregate_data,
                      ctrl)

  # validate inputs #
  check_targets(target_data, ctrl)
  check_modifiers(modifier_data, ctrl) 
  check_aggregates(aggregate_data, ctrl)
  check_item(item_data, ctrl)

  # restrict data #
  item_data <- restrict_items(item_data, ctrl)
  ctrl@item_names <- intersect(ctrl@item_names, names(item_data))
  modifier_data <- restrict_modifier(item_data, modifier_data, ctrl)
  aggregate_data <- restrict_aggregates(aggregate_data, ctrl)
  ctrl@aggregate_item_names <-
    ctrl@aggregate_item_names[ctrl@aggregate_item_names %chin%
                              aggregate_data$item]
  d_in$time_observed <- get_observed(item_data, aggregate_data, ctrl@time_name)
  d_in$geo_observed <- get_observed(item_data, aggregate_data, ctrl@geo_name)

  # aggregate individual item response data to group level #
  weight(item_data, target_data, ctrl)
  d_in$gt_items <- discretize(item_data, ctrl)

  d_in$group_grid <- make_group_grid(item_data, aggregate_data, ctrl)
  d_in$group_grid_t <- make_group_grid_t(d_in$group_grid, ctrl)
  d_in$group_counts <- make_group_counts(item_data, aggregate_data, d_in, ctrl)

  # make objects used by dgirt.stan #
  d_in$n_vec <- setNames(d_in$group_counts$n_grp, d_in$group_counts$name)
  d_in$s_vec <- setNames(d_in$group_counts$s_grp, d_in$group_counts$name)

  d_in$MMM <- get_missing_groups(d_in$group_counts, d_in$group_grid, ctrl)

  d_in$G <- nrow(d_in$group_grid_t)
  d_in$G_hier <- ifelse(!length(modifier_data), nlevels(gl(1L, d_in$G)),
                        max(unlist(length(ctrl@modifier_names)), 1L))
  d_in$T <- length(ctrl@time_filter)
  d_in$Q <- length(c(intersect(ctrl@item_names, names(item_data)),
                     intersect(ctrl@aggregate_item_names,
                               unique(aggregate_data$item))))

  d_in$WT <- array(1, dim = c(d_in$T, d_in$G_hier, d_in$G))

  d_in$l2_only <- matrix(0L, nrow = length(ctrl@time_filter), ncol = d_in$Q)
  d_in$NNl2 <- array(0L, dim = c(d_in$T, d_in$Q, d_in$G_hier))
  d_in$SSl2 <- d_in$NNl2

  d_in$XX <- make_design_matrix(item_data, d_in, ctrl)
  d_in$ZZ <- shape_hierarchical_data(item_data, modifier_data, d_in, ctrl)
  d_in$ZZ_prior <- d_in$ZZ
  d_in$hier_names <- dimnames(d_in$ZZ)[[2]]

  d_in$D <- ifelse(ctrl@constant_item, 1L, d_in$T)
  d_in$N <- nrow(d_in$group_counts)
  d_in$P <- ncol(d_in$ZZ)
  d_in$S <- dim(d_in$ZZ)[[2]]
  d_in$H <- dim(d_in$ZZ)[[3]]
  d_in$Hprior <- d_in$H 

  # include subset data and other objects that may be useful later #
  d_in$item_data <- item_data
  d_in$modifier_data <- modifier_data
  d_in$aggregate_data <- aggregate_data
  d_in$target_data <- target_data
  d_in$control <- ctrl
  d_in$call <- match.call()

  # validate input to model #
  check_dimensions(d_in)
  check_values(d_in)
  check_order(d_in)

  d_in
}

get_missing_groups <- function(group_counts, group_grid, ctrl) {
  all_group_counts <- merge(group_counts, group_grid, all = TRUE,
                            by = c(ctrl@group_names, ctrl@geo_name, ctrl@time_name))
  all_group_counts[, ("is_missing") := is.na(n_grp) + 0L]
  all_group_counts[is.na(n_grp), c("n_grp", "s_grp") := 1L]
  all_group_counts[, (ctrl@geo_name) := paste0("x_", .SD[[ctrl@geo_name]]), .SDcols = c(ctrl@geo_name)]
  acast_formula <- as.formula(paste0(ctrl@time_name, "~ item ~", paste(ctrl@group_names, collapse = "+"), "+", ctrl@geo_name))
  MMM <- reshape2::acast(all_group_counts, acast_formula, value.var = "is_missing", fill = 1)
  # merging group_grid included unobserved combinations of grouping variables; being unobserved, they're associated with
  # no item, and when the result is cast to array, NA will appear in the second dimension as an item name
  MMM <- MMM[, dimnames(MMM)[[2]] != "NA", , drop = FALSE]
  stopifnot(all(MMM %in% c(0, 1)))
  MMM
}

shape_hierarchical_data <- function(item_data, modifier_data, d_in, ctrl) {
  if (d_in$G_hier == 1) {
    zz.names <- list(ctrl@time_filter, dimnames(d_in$XX)[[2]], "")
    zz <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  } else {
    # the array of hierarchical data ZZ should be T x P x H, where T is the number of time periods, P is the number of
    # hierarchical parameters (including the geographic), and H is the number of predictors for geographic unit effects
    # TODO: make flexible; as written we must model geo x t
    modeled_params = c(ctrl@geo_name, ctrl@time_name)
    unmodeled_params = setdiff(c(ctrl@geo_name, ctrl@time_name, ctrl@group_names), modeled_params)

    # does hierarchical data include all observed geo?
    missing_modifier_geo <- setdiff(
                                    unique(d_in$group_grid_t[[ctrl@geo_name]]),
                                    unique(modifier_data[[ctrl@geo_name]]))
    if (length(missing_modifier_geo)) stop("no hierarchical data for geo in item data: ", missing_modifier_geo)
    # does hierarchical data include all modeled t?
    missing_modifier_t <- setdiff(ctrl@time_filter, unique(modifier_data[[ctrl@time_name]]))
    if (length(missing_modifier_t)) stop("missing hierarchical data for t in item data: ", missing_modifier_t)

    # NOTE: we're prepending the value of geo with its variable name; may not be desirable
    hier_frame <- copy(modifier_data)[, ctrl@geo_name := paste0(ctrl@geo_name, modifier_data[[ctrl@geo_name]])]
    hier_frame[, setdiff(names(hier_frame), c(modeled_params, modifier_names)) := NULL, with = FALSE]
    hier_frame[, c("param", ctrl@geo_name) := .(hier_frame[[ctrl@geo_name]], NULL), with = FALSE]
    setkeyv(hier_frame, c("param", ctrl@time_name))

    modeled_param_names <- unique(hier_frame[, param])
    unmodeled_param_levels = unlist(lapply(unmodeled_params, function(x) {
                                             paste0(x, unique(d_in$group_grid_t[[x]]))[-1]
        }))
    param_levels <- c(modeled_param_names, unmodeled_param_levels)

    unmodeled_frame <- expand.grid(c(list(
                                          unmodeled_param_levels,
                                          sort(unique(hier_frame[[ctrl@time_name]])),
                                          as.integer(rep(list(0), length(modifier_names))))))
    unmodeled_frame <- setNames(unmodeled_frame, c("param", ctrl@time_name, modifier_names))
    setDT(unmodeled_frame, key = c("param", ctrl@time_name))

    hier_frame <- rbind(hier_frame, unmodeled_frame)

    # FIXME: hacky handling of the possibility of NA modifier values here
    hier_frame[, c(modifier_names) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = modifier_names]

    hier_melt = melt(hier_frame, id.vars = c("param", ctrl@time_name), variable.name = "modifiers", variable.factor = FALSE,
                     value.factor = FALSE)
    setkeyv(hier_melt, c("param", "year"))

    melt_formula <- as.formula(paste(ctrl@time_name, "param", "modifiers", sep = " ~ "))
    zz <- reshape2::acast(hier_melt, melt_formula, drop = FALSE, value.var = "value")
    zz <- zz[, -1, , drop = FALSE]
  } 
  zz
}

make_design_matrix <- function(item_data, d_in, ctrl) {
  design_formula <- as.formula(paste("~ 0", ctrl@geo_name,
                                     paste(ctrl@group_names, collapse = " + "),
                                     sep = " + "))
  design_matrix <- with_contr.treatment(model.matrix(design_formula, d_in$group_grid_t))
  rownames(design_matrix) <- paste(paste(d_in$group_grid_t[[ctrl@group_names]], sep = "_"),
                                   d_in$group_grid_t[[ctrl@geo_name]],  sep = "_x_")
  design_matrix <- subset(design_matrix, select = -1)
  # TODO: move to S4 validate
  invalid_values <- setdiff(as.vector(design_matrix), c(0, 1))
  if (length(invalid_values)) {
    stop("design matrix values should be in (0, 1); found ",
         paste(sort(invalid_values), collapse = ", "))
  }
  design_matrix
}

with_contr.treatment <- function(...) {
  contrast_options = getOption("contrasts")
  options("contrasts"= c(unordered = "contr.treatment",
                         ordered = "contr.treatment"))
  res <- eval(...)
  options("contrasts"= contrast_options)
  res
}

calc_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}
