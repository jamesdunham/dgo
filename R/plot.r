#' Plot group means
#'
#' @param theta_bars Result of either `dgirt` if `method = "optimize"` or `extract_dgirt` if `method = "rstan"` (the default).
#' @param time_id Variable in theta_bars representing time, which will be used appear on the horizontal axis. A character vector.
#' @param facet_var Name(s) of variable(s) in theta_bars representing groups that will be plotted in separate facets. A character vector.
#' @param jitter Logical; should points be jittered?
#' @import ggplot2
#' @export
#' @examples
#' data(optimized_theta_bars)
#' optimized_theta_bars$year <- as.integer(optimized_theta_bars$year)
#' plot_means(optimized_theta_bars, "year", "state", jitter = TRUE)
plot_means <- function(theta_bars, time_id, facet_var, jitter = FALSE) {
  assertthat::assert_that(is.data.frame(theta_bars))
  assertthat::assert_that(assertthat::is.string(time_id))
  assertthat::assert_that(assertthat::is.string(facet_var))
  assertthat::assert_that(assertthat::has_name(theta_bars, time_id))
  assertthat::assert_that(assertthat::has_name(theta_bars, facet_var))
  assertthat::assert_that(assertthat::has_name(theta_bars, "value"))
  assertthat::assert_that(is.numeric(theta_bars[[time_id]]))
  assertthat::assert_that(is.numeric(theta_bars[["value"]]))
  p <- ggplot2::ggplot(theta_bars, ggplot2::aes_string(x = time_id, y = "value")) +
    ggplot2::geom_smooth(se = FALSE, method = "gam") +
    ggplot2::facet_wrap(facet_var) +
    ggplot2::ylab('estimate') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=270, vjust=0.5))
  if (isTRUE(jitter)) {
    p <- p + ggplot2::geom_point(shape = 1, alpha = 1/4, position =
      ggplot2::position_jitter(width = .05, height = .05))
  } else {
    p <- p + ggplot2::geom_point(shape = 1, alpha = 1/4)
  }
  return(p)
}
