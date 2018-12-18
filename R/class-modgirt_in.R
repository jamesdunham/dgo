#' A class for item response data shaped for modgirt
#'
#' @slot items Names of item response variables.
#' @slot time Name of a time variable.
#' @slot geo Name of a geographic variable.
#' @slot demo Names of further grouping variables, usually demographic.
#' @slot stan_data Data shaped for estimation.
#'
#' @aliases modgirt_in modgirt_in-class
#' @name modgirt_in-class
#' @export
modgirt_in <- setClass("modgirt_in",
  slots = list(
    items = "character",
    time = "character",
    geo = "character",
    demo = "character",
    stan_data = "list")
)

#' @describeIn modgirt_in Methods
#' @param object An object of class \code{Individual}
#' @param ... Further arguments.
setMethod("summary", "modgirt_in", function(object, ...) {
    cat("Item response data:\n")
    cat("Items:\n")
    print(object@items)
    cat("Respondents:\n")
    print(nrow(object@stan_data))
    cat("Grouping variables:\n")
    print(c(object@time, object@geo, object@demo))
})

#' @describeIn modgirt_in Methods
setMethod("show", "modgirt_in", function(object) {
  summary(object)
})
