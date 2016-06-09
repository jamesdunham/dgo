\docType{class}
\name{dgirtfit-class}
\alias{as.data.frame.dgirtfit}
\alias{dgirtfit}
\alias{dgirtfit-class}
\alias{get_posterior_mean,dgirtfit-method}
\alias{print}
\alias{print,dgirtfit-method}
\alias{print.dgirtfit}
\alias{show,dgirtfit-method}
\alias{summarize}
\alias{summarize,dgirtfit-method}
\alias{summary,dgirtfit-method}
\title{\code{dgirtfit}: a class for fitted DGIRT models}
\usage{
\S4method{show}{dgirtfit}(object)

print(x, ...)
print.dgirtfit(x, ...)
\S4method{print}{dgirtfit}(x, ...)


\S4method{summary}{dgirtfit}(object, ..., verbose = FALSE)

\S4method{get_posterior_mean}{dgirtfit}(object, pars = "theta_bar", ...)

summarize(x, ...)
\S4method{summarize}{dgirtfit}(x, pars = "theta_bar", funs = c("mean", "sd",
  "median", "q_025", "q_975"))

\method{as.data.frame}{dgirtfit}(x, ..., pars = "theta_bar", discard = TRUE,
  keep.rownames = FALSE)
}
\arguments{
\item{object, x}{A \code{dgirtfit-class} object.}

\item{...}{Further arguments to \code{\link{stanfit-class}} methods.}

\item{verbose}{Whether to show the verbose RStan summary.}

\item{pars}{Selected parameter names.}

\item{funs}{Quoted names of summary functions. `q_025` is accepted as
shorthand for `function(x) quantile(x, .025)`, and similarly `q_975`.}

\item{discard}{Whether to discard samples from warmup iterations.}

\item{keep.rownames}{Whether to retain original parameter names with numeric
indexes, as output from RStan.}
}
\description{
Fitting a dgirt model results in a \code{dgirtfit} object that inherits from
\code{\link{rstan}}'s \code{\link[rstan]{stanfit-class}}. 

The \code{rstan} methods will be dispatched only if a \code{dgirtfit} method
does not exist

Descriptive labels for parameters on time periods, local geographic areas, and
grouping variables will be added to most output.

\code{show} method for \code{dgirtfit-class} objects

S4 \code{print} generic

\code{print} method for \code{dgirtfit-class} objects

\code{print} method for \code{dgirtfit-class} objects

\code{summary} method for \code{dgirtfit-class} objects

\code{get_posterior_mean} method for \code{dgirtfit-class} objects

S4 \code{summarize} generic

\code{summarize} method for \code{dgirtfit-class} objects

\code{as.data.frame} method for \code{dgirtfit-class} objects
}
\section{Slots}{

\describe{
\item{\code{dgirt_in}}{\code{\link{dgirtin-class}} data used to fit the model.}
}}
\examples{
data(toy_dgirtfit)
# summarize the fitted results
summary(toy_dgirtfit, pars = 'xi')

# get posterior means with a convenience function
get_posterior_mean(toy_dgirtfit, pars = 'theta_bar')

# generally apply functions to posterior samples after warmup; n.b.
# `as.array` is iterations x chains x parameters so `MARGIN = 3` applies
# `FUN` over iterations and chains
apply(as.array(toy_dgirtfit, pars = 'xi'), 3, mean)

# access the posterior samples
as.array(toy_dgirtfit, pars = 'theta_bar')
as.data.frame(toy_dgirtfit, pars = 'theta_bar')
extract(toy_dgirtfit, pars = 'theta_bar')
toy_dgirtfit
print(toy_dgirtfit)
print(toy_dgirtfit)
summary(toy_dgirtfit)
summary(toy_dgirtfit, pars = "theta_bar", verbose = TRUE)
get_posterior_mean(toy_dgirtfit)
summarize(toy_dgirtfit)
summarize(toy_dgirtfit, pars = "xi")
summarize(toy_dgirtfit, funs = "mean")
as.data.frame(toy_dgirtfit)
as.data.frame(toy_dgirtfit, keep.rownames = TRUE)
}
\seealso{
\code{\link{stanfit-class}} \code{\link{dgirtin-class}}
}

