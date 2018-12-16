#' A class for MODGIRT model data
#'
#' \code{shape_multinomial()} generates objects of class \code{modgirt_in} for
#' modeling with \code{modgirt()}.
#'

#' @aliases modgirt_fit modgirt_fit-class
#' @name modgirt_fit-class
modgirt_fit <- setClass("modgirt_in", slots = list(
    stan_data = list)
)

