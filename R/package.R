#' dgo: Dynamic Estimation of Group-level Opinion
#'
#' Fit dynamic group-level IRT and MRP models from individual or aggregated item
#' response data. This package handles common preprocessing tasks and extends
#' functions for inspecting results, poststratification, and quick iteration
#' over alternative models.
#'
#' @docType package
#' @name dgo
#' @import data.table
#' @import dgodata
#' @import ggplot2
#' @import methods
#' @importFrom lubridate seconds_to_period
#' @importFrom stats as.formula formula model.frame model.matrix na.fail na.omit
#'   quantile sd setNames weighted.mean weights
#' @importFrom utils capture.output type.convert packageVersion
NULL
