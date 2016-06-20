#' @rdname plot-method
setGeneric("dgirt_plot", signature = "x", function(x, ...)
           standardGeneric("dgirt_plot"))

#' \code{dgirt_plot}: plot \code{dgirtfit}-class objects
#'
#' @param x A \code{dgirtfit-class} object.
#' @param y_fun Summary function to be plotted as \code{y}.
#' @param y_min Summary function giving the \code{ymin} argument for a
#' \code{geom_pointrange} object.
#' @param y_max Summary function giving the \code{ymax} argument for a
#' \code{geom_pointrange} object.
#' @param pars Selected parameter. 
#' @rdname plot-method
#' @examples
#' dgirt_plot(toy_dgirtfit) 
#' dgirt_plot(toy_dgirtfit, y_min = NULL, y_max = NULL)
#' p <- dgirt_plot(toy_dgirtfit)
#' p %+% ylab("posterior median")
setMethod("dgirt_plot", signature(x = "dgirtfit"),
  function(x, y_fun = "median", y_min = "q_025", y_max = "q_975", pars =
           "theta_bar") {
  stopifnot(length(pars) == 1L)

  ctrl <- x@dgirt_in$control
  samples <- summarize(x, funs = c(y_fun, y_min, y_max))

  plot_internal(samples, ctrl@group_names, ctrl@time_name, ctrl@geo_name, y_fun,
                y_min, y_max)
})

#' \code{dgirt_plot}: plot \code{data.frame} objects
#'
# #' @param x A \code{dgirtfit-class} object.
#' @param group_names Discrete grouping variables, if any, which will be used as
#' the \code{color} argument in \code{aes}.
#' @param time_name A time variable with numeric values that will be plotted on
#' the x axis.
#' @param geo_name A variable representing local areas that will be used in
#' faceting.
# #' @param y_fun Summary function to be plotted as \code{y}.
# #' @param y_min Summary function giving the \code{ymin} argument for a
# #' \code{geom_pointrange} object.
# #' @param y_max Summary function giving the \code{ymax} argument for a
# #' \code{geom_pointrange} object.
# #' @param pars Selected parameter. 
#' @rdname plot-method
#' @examples
#' data(state_year_targets)
#' ps <- poststratify(toy_dgirtfit, state_year_targets, strata_names =
#'                    c("state", "year"), aggregated_names = "race")
#' dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
setMethod("dgirt_plot", signature(x = "data.frame"),
  function(x, group_names, time_name, geo_name, y_fun = "median", y_min =
           "q_025", y_max = "q_975") {
    if (length(y_fun)) {
      x <- x[, do_funs(list(value), c(y_fun, y_min, y_max)),
             by = c(group_names, time_name, geo_name)]
      data.table::setnames(x, grep("^V\\d+", names(x),value = TRUE),
                           c(y_fun, y_min, y_max))
    } 
    plot_internal(x, group_names, time_name, geo_name, y_fun, y_min, y_max)
})

plot_internal <- function(samples, group_names, time_name, geo_name, y_fun,
                          y_min, y_max) {

  if (!length(y_fun) & (length(y_min) | length(y_max))) {
    stop("If y_fun is unused then y_min and y_max should be too.")
  } else if (length(y_min) & !length(y_max)) {
    stop("If y_min is used then y_max should be too.")
  } else if (!length(y_min) & length(y_max)) {
    stop("If y_max is used then y_min should be too.")
  }

  p <- ggplot2::ggplot(data = samples,
         ggplot2::aes_string(x = time_name, y = y_fun, color = group_names)) +
       ggplot2::geom_line(alpha = 0.7) +
       ggplot2::facet_wrap(geo_name) + 
       ggplot2::theme_bw() +
       ggplot2::scale_x_continuous(minor_breaks = NULL, breaks =
                                   unique(samples[[time_name]]))

  if (length(y_min) & length(y_max)) {
    p <- p + ggplot2::geom_pointrange(
               ggplot2::aes_string(y = y_fun, ymin = y_min, ymax = y_max),
               alpha = 0.7)
  } else {
    p <- p + ggplot2::geom_line(alpha = 0.7, ggplot2::aes_string(y = y_fun))
  }
  p
}

#' \code{plot} plot method for \code{dgirtfit}-class objects
#'
#' @param y Ignored.
#' @param ... Further arguments to \code{\link{dgirt_plot}}.
#'
#' @rdname plot-method
#' @examples
#'
#' plot(toy_dgirtfit)
setMethod("plot", signature(x = "dgirtfit", y = "missing"),
          function(x, ...) {
            dgirt_plot(x, ...)
          })

