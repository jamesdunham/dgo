#' A Minimal Example of the dgirtIn Class.
#'
#' `\link{shape}` returns a `dgirtIn`-class object used with `\link{dgirt}` for
#' DGIRT modeling. `toy_dgirt_in` is a minimal `dgirtIn` object, mostly for
#' use in development.
#'
#' @docType data
#' @name toy_dgirt_in
#' @usage toy_dgirt_in
#' @format A `dgirtIn` object.
#' @examples
#' # created as follows
#' data(state_opinion)
#' toy_dgirt_in <- shape(state_opinion,
#'                  time_name = "year",
#'                  item_names = c("Q_cces2006_minimumwage",
#'                                 "Q_cces2006_abortion"),
#'                  geo_name = "state",
#'                  group_names = "race",
#'                  survey_name = "source",
#'                  weight_name = "weight")
#' @include shape.r
#' toy_dgirt_in
NULL
