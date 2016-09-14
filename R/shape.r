#' \code{shape}: prepare data for modeling with \code{dgirt}
#'
#' This function shapes various kinds of data for use in a dgirt model. Most
#' arguments give the name or names of key variables in the data; they end in
#' \code{_name} or \code{_names} and should be character vectors. Some others
#' implement preprocessing and modeling choices.

#' @section Modifier Data:
#' Geographic hierarchical parameters can be modeled with \code{modifier_data}.
#' These arguments are also required:
#' \describe{
#'   \item{\code{modifier_names}:}{Modifiers of geographic hierarchical
#'   parameters, e.g. median household income in each local-area and
#'   time-period combination.}
#'   \item{\code{t1_modifier_names}:}{Modifiers to be used instead of those in
#'   \code{modifier_names}, only in the first period.}
#'   \item{\code{standardize}:}{Whether to standardize hierarchical modifier
#'   data to be zero-mean and unit-variance for performance gains. For
#'   discussion see the Stan Language Reference section "Standardizing
#'   Predictors and Outputs."}
#' }

#' @section Aggregate Data:
#' Specifying \code{aggregate_data} requires no additional arguments; instead,
#' we make many assumptions about the data. This implementation is likely to
#' change in the future.
#'
#' \code{aggregate_data} is expected to be a long table of trial and success
#' counts by group and item. Some variable names given for \code{item_data} are
#' expected in the table of aggregates: \code{group_names}, \code{geo_name}, and
#' \code{time_name}. Three fixed variable names are also expected in
#' \code{aggregate_data}: \code{item} giving item identifiers, \code{n_grp}
#' giving adjusted counts of item-response trials, and \code{s_grp} giving
#' adjusted counts of item-response successes. The counts should be adjusted
#' consistently with the transformations applied to the individual
#' \code{item_data}.

#' @section Preprocessing:
#' If \code{target_data} is specified \code{shape} will adjust the weighting of
#' groups toward population targets via raking. This relies on an adaptation of
#' \code{\link[survey]{rake}}. The additional required arguments are
#' \code{target_proportion_name} and \code{raking}. 
#'
#' \code{shape} can restrict data row-wise in \code{item_data},
#' \code{modifier_data}, and \code{aggregate_data} to that within specified time
#' periods (\code{time_filter}) and local geographic areas (\code{geo_filter}).
#' Data can also be filtered for sparsity, to keep items that appear in a
#' minimum of time periods or surveys. This is a column-wise operation. If both
#' row-wise and column-wise restrictions are specified, \code{shape} iterates
#' over them until they leave the data unchanged.

#' @section Target Data:
#' \describe{
#'   \item{target_proportion_name}{The variable giving population proportions
#'   for strata.}
#'   \item{raking}{A formula or list of formulas specifying the variables on
#'   which to rake.}
#'   \item{geo_filter}{A character vector giving values of the geographic
#'   variable. Defaults to observed values.}
#'   \item{time_filter}{A numeric vector giving possible values of the time
#'   variable. Observed and unobserved time periods can be given. Defaults to
#'   observed values.}
#'   \item{min_survey_filter}{An integer minimum of survey appearances for
#'   included items. Defaults to 1.}
#'   \item{min_t_filter}{An integer minimum of time period appearances for
#'   included items. Defaults to 1.}
#' }

#' @section Modeling Choices:
#' Optional. Most arguments like this one are now in the \code{dgirt} signature,
#' but \code{constant_item} affects the shape of the data. It may move to
#' \code{dgirt} in the future.
#' \describe{
#'   \item{constant_item}{Whether item difficulty parameters should be constant
#'   over time. Default \code{TRUE}.}
#' }
#'
#' @param item_data A table in which items appear in columns and each row
#' represents an individual's responses in some time period and local geographic
#' area.
#'
#' @param item_names Individual item responses. These variables should be
#'   integers or ordered factors in the data.
#'
#' @param group_names Discrete grouping variables, usually demographic. Using
#'   numeric variables is allowed but not recommended.
#'
#' @param geo_name A geographic variable representing local areas.
#'
#' @param time_name A time variable with numeric values.
#'
#' @param survey_name A survey identifier.
#'
#' @param weight_name A variable giving survey weights.
#'
#' @param modifier_data Table giving characteristics of local geographic areas
#' in time periods. See details below.
#'
#' @param target_data A table giving population proportions for groups by local
#' geographic area and time period. See details below.
#'
#' @param raking A formula or list of formulas specifying the variables on which
#' to rake survey weights.
#'
#' @param aggregate_data A table of trial and success counts by group and item.
#' See details below.
#'
#' @param aggregate_item_names A subset of values of the \code{item} variable in
#' \code{aggregate_data}, for restricting the aggregate data.
#'
#' @param id_vars Additional variables that should be included in the result,
#' other than those specified elsewhere.
#'
#' @param ... Further arguments for more complex models, input data, and
#' preprocessing.
#'
#' @return An object of class \code{dgirtIn} expected by \code{\link{dgirt}}.
#'
#' @examples
#' # model individual item responses
#' data(opinion)
#' opinion$respondent = 1:nrow(opinion)
#' shaped_responses <- shape(opinion,
#'                           item_names = "Q_cces2006_gaymarriageamendment",
#'                           time_name = "year",
#'                           geo_name = "state",
#'                           group_names = "race",
#'                           weight_name = "weight",
#'                           survey_name = "source",
#'                           id_vars = 'respondent')
#' # summarize result)
#' summary(shaped_responses)
#'
#' # check sparseness of data to be modeled
#' get_item_n(shaped_responses, by = "year")
#'
#' @import data.table
#' @seealso \code{\link{dgirtin-class}} \code{\link{dgirtfit-class}}
#' @include require_namespace.r
#' @export
shape <- function(item_data,
                  item_names,
                  time_name,
                  geo_name,
                  group_names = NULL,
                  weight_name,
                  raking = NULL,
                  survey_name,
                  modifier_data = NULL,
                  target_data = NULL,
                  aggregate_data = NULL,
                  aggregate_item_names = NULL,
                  id_vars = NULL,
                  ...) {

  ctrl <- init_control(item_data, item_names, time_name, geo_name, group_names,
                       weight_name, survey_name, raking, id_vars, aggregate_data, 
                       aggregate_item_names, ...)
  d_in <- dgirtIn$new(ctrl)

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

  # rake survey weights #

  if (length(target_data)) {
    item_data <- weight(item_data, target_data, ctrl)
    ctrl@weight_name <- "raked_weight"
  }

  # aggregate individual item response data to group level #
  item_data <- dichotomize(item_data, ctrl)
  # this assignment should be redundant, but without it some variables created
  # in dichotomize() weren't appearing in item_data 

  d_in$group_grid <- make_group_grid(item_data, aggregate_data, ctrl)
  d_in$group_grid_t <- make_group_grid_t(d_in$group_grid, ctrl)
  d_in$group_counts <- make_group_counts(item_data, aggregate_data, ctrl)
  d_in$gt_items <- unique(d_in$group_counts$item)

  # make objects used by dgirt.stan #
  d_in$observed <- which(d_in$group_counts[["n_grp"]] > 0)
  d_in$N_observed <- length(d_in$observed)
  d_in$n_vec <- setNames(d_in$group_counts$n_grp, d_in$group_counts$name)
  d_in$s_vec <- setNames(d_in$group_counts$s_grp, d_in$group_counts$name)

  d_in$G <- nrow(unique(d_in$group_counts[, c(ctrl@geo_name, ctrl@group_names),
                        with = FALSE]))
  d_in$G_hier <- ifelse(!length(modifier_data), nlevels(gl(1L, d_in$G)),
                        max(unlist(length(ctrl@modifier_names)), 1L))
  d_in$T <- length(ctrl@time_filter)
  d_in$Q <- length(d_in$gt_items)

  d_in$WT <- array(1, dim = c(d_in$T, d_in$G_hier, d_in$G))

  d_in$l2_only <- matrix(0L, nrow = length(ctrl@time_filter), ncol = d_in$Q)
  d_in$NNl2 <- array(0L, dim = c(d_in$T, d_in$Q, d_in$G_hier))
  d_in$SSl2 <- d_in$NNl2

  d_in$XX <- make_design_matrix(d_in, ctrl)
  d_in$ZZ <- shape_hierarchical_data(item_data, modifier_data, d_in, ctrl,
                                     t1 = FALSE)
  d_in$ZZ_prior <- shape_hierarchical_data(item_data, modifier_data, d_in, ctrl,
                                           t1 = TRUE)
  d_in$hier_names <- dimnames(d_in$ZZ)[[2]]

  d_in$D <- ifelse(ctrl@constant_item, 1L, d_in$T)
  d_in$N <- nrow(d_in$group_counts)
  d_in$P <- ncol(d_in$ZZ)
  d_in$S <- length(unique(d_in$group_grid[[ctrl@geo_name]])) - 1
  d_in$H <- dim(d_in$ZZ)[[3]]
  d_in$Hprior <- d_in$H

  # include subset data and other objects that may be useful later #
  d_in$item_data <- item_data
  d_in$modifier_data <- modifier_data
  d_in$aggregate_data <- aggregate_data
  d_in$target_data <- target_data
  d_in$control <- ctrl
  d_in$call <- match.call()
  d_in$pkg_version <- packageVersion("dgirt")

  # validate input to model #
  check_dimensions(d_in)
  check_values(d_in)
  check_names(d_in)

  d_in
}

shape_hierarchical_data <- function(item_data, modifier_data, d_in, ctrl, t1) {
    modifier_names <- ifelse(t1, ctrl@t1_modifier_names, ctrl@modifier_names)
    if (!length(modifier_data) | is.na(modifier_names)) {
      zz.names <- list(ctrl@time_filter, dimnames(d_in$XX)[[2]], "")
      zz <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
    } else {
    # the array of hierarchical data ZZ should be T x P x H, where T is the
    # number of time periods, P is the number of hierarchical parameters
    # (including the geographic), and H is the number of predictors for
    # geographic unit effects

    hier_frame <- data.table::copy(modifier_data)

    extra_colnames <- setdiff(names(hier_frame),
                             c(ctrl@geo_name, ctrl@time_name, modifier_names))
    if (length(extra_colnames)) {
      hier_frame[, c(extra_colnames) := NULL, with = FALSE]
    }

    # NOTE: We create param by renaming geo_name. Thus the requirement to model
    # geographic predictors.
    hier_frame[, c("param", ctrl@geo_name) := list(hier_frame[[ctrl@geo_name]],
                                                   NULL), with = FALSE]
    data.table::setkeyv(hier_frame, c("param", ctrl@time_name))
    all(ctrl@time_filter %in% hier_frame$D_year)

    # "param" is just the unique values of the geo var at the moment
    modeled_param_names <- unique(hier_frame[, get("param")])

    # unmodeled param levels will be those of groups
    unmodeled_param_levels = unlist(lapply(d_in$unmod_par_names, function(x) {
                                             paste0(x, unique(d_in$group_grid_t[[x]]))[-1]
        }))
    param_levels <- c(modeled_param_names, unmodeled_param_levels)

    # make a zeroed table for unmodeled parameters by time period
    unmodeled_frame <- expand.grid(c(list(unmodeled_param_levels,
                                          ctrl@time_filter), rep(list(0L),
                                          length(modifier_names))))
    unmodeled_frame <- setNames(unmodeled_frame, c("param", ctrl@time_name,
                                                   modifier_names))
    data.table::setDT(unmodeled_frame, key = c("param", ctrl@time_name))

    hier_frame <- rbind(hier_frame, unmodeled_frame)

    zz <- sapply(modifier_names, function(x) { 
                   matrix(hier_frame[[x]],
                          # We have T rows, so filling by column is correct SO
                          # LONG AS time varies fastest then param in
                          # hier_frame
                          nrow = length(unique(hier_frame[[ctrl@time_name]])),
                          ncol = length(unique(hier_frame$param)),
                          dimnames = list(unique(hier_frame[[ctrl@time_name]]),
                                          unique(hier_frame$param)))
                        }, simplify = 'array')
    # omit first geo parameter
    zz <- zz[, -1, , drop = FALSE]
  }
  zz
}

make_design_matrix <- function(d_in, ctrl) {
  design_formula <- paste("~ 0", ctrl@geo_name, sep = " + ")
  if (length(ctrl@group_names)) {
    design_formula <- paste(design_formula, ctrl@group_names, collapse = " + ",
                            sep = " + ")
  }
  design_formula = as.formula(design_formula)
  design_matrix <- with_contr.treatment(model.matrix(design_formula,
                                                     d_in$group_grid_t))
  
  rn <- do.call(function(...) paste(..., sep = "__"),
                d_in$group_grid_t[, c(ctrl@geo_name, ctrl@group_names),
                                  with = FALSE])
  rownames(design_matrix) <- rn
  colnames(design_matrix) <- sub(paste0("^", ctrl@geo_name), "",
                                 colnames(design_matrix))

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
