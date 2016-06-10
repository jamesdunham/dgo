\name{dgirtin-class}

\docType{class}
\title{Class \code{dgirtIn}: data prepared for modeling with \code{dgirt}}

\alias{dgirtin-class}
\alias{get_item_n}
\alias{get_item_n,dgirtIn-method}
\alias{get_item_names}
\alias{get_item_names,dgirtIn-method}
\alias{get_n}
\alias{get_n,dgirtIn-method}
\alias{show,dgirtIn-method}
\alias{summary,dgirtIn-method}
\alias{print.dgirtIn}
\alias{print,dgirtIn-method}

\usage{
\S4method{summary}{dgirtIn}(object, ...)
\S4method{get_item_names}{dgirtIn}(x)
\S4method{get_n}{dgirtIn}(x, by = NULL, aggregate_name = NULL)
\S4method{get_item_n}{dgirtIn}(x, by = NULL, aggregate_data = FALSE)
\S4method{show}{dgirtIn}(object)
\S4method{print}{dgirtIn}(x, ...)
\method{print}{dgirtIn}(x, ...)
}

\arguments{
\item{object}{An object of class \code{dgirtIn} as returned by \code{shape}.}
\item{...}{Unused.}
\item{x}{An object of class \code{dgirtIn} as returned by \code{shape}.}
\item{by}{The name of a grouping variable.}
\item{aggregate_name}{If specified \code{get_n} will operate on the table passed
to \code{shape} as \code{aggregate_data} instead of on the individual data and count
nonmissingness in the given variable.}
\item{aggregate_data}{If specified \code{get_n} will operate on the table passed
to \code{shape} as \code{aggregate_data} instead of on the individual data.}
}

\value{
A list of item names.
}

\section{Slots}{
\describe{
\item{\code{control}}{Arguments from the call to \code{shape}.}
}
}

\description{
\code{shape} generates objects of class \code{dgirtIn} for modeling with
\code{dgirt}.
}
\examples{
get_item_names(toy_dgirt_in)

get_n(toy_dgirt_in)
get_n(toy_dgirt_in, by = "year")
get_n(toy_dgirt_in, by = "source")

get_item_n(toy_dgirt_in)
get_item_n(toy_dgirt_in, by = "year")
}

\seealso{
`\link{get_item_n}, \link{get_item_names}`
}

