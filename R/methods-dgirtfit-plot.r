#' \code{dgirt_plot} plot \code{dgirtfit}-class objects
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
dgirt_plot <- function(x, y_fun = "median", y_min = "q_025", y_max = "q_975",
                          pars = "theta_bar") {
  stopifnot(length(pars) == 1L)

  ctrl <- x@dgirt_in$control
  samples <- summarize(x, funs = c(y_fun, y_min, y_max))

  p <- ggplot2::ggplot(data = samples,
         ggplot2::aes_string(x = ctrl@time_name, y = y_fun, color =
                             ctrl@group_names)) +
       ggplot2::geom_line(alpha = 0.7) +
       ggplot2::facet_wrap(ctrl@geo_name) + 
       ggplot2::theme_bw() +
       ggplot2::scale_x_continuous(minor_breaks = NULL, breaks =
                                   unique(samples[[ctrl@time_name]]))

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
#' @param x An object of class \code{dgirtfit}.
#' @param ... Further arguments to \link\code{dgirt_plot}}.
#'
#' @rdname plot-method
#' @examples
#'
#' plot(toy_dgirtfit)
setMethod("plot", signature(x = "dgirtfit", y = "missing"),
          function(x, ...) {
            dgirt_plot(x, ...)
          })

