\name{dgirtfit-class}

\docType{class}
\title{Class \code{dgirtfit}: fitted DGIRT model}

\alias{as.data.frame.dgirtfit}
\alias{dgirtfit}
\alias{dgirtfit-class}
\alias{get_posterior_mean,dgirtfit-method}
\alias{print,dgirtfit-method}
\alias{print-method}
\alias{show,dgirtfit-method}
\alias{summarize}
\alias{summarize,dgirtfit-method}
\alias{summary,dgirtfit-method}


\usage{
\S4method{show}{dgirtfit}(object)
\S4method{print}{dgirtfit}(x, ...)
\S4method{summary}{dgirtfit}(object, ..., verbose = FALSE)
\S4method{get_posterior_mean}{dgirtfit}(object, pars = "theta_bar", ...)
\S4method{summarize}{dgirtfit}(x, pars = "theta_bar", funs = c("mean", "sd",
  "median", "q_025", "q_975"))
\method{as.data.frame}{dgirtfit}(x, ..., pars = "theta_bar",
  keep.rownames = FALSE)
summarize(x, ...)
}

\arguments{
\item{object, x}{A \code{dgirtfit-class} object.}
\item{...}{Further arguments to \code{\link{stanfit-class}} methods.}
\item{verbose}{Whether to show the verbose RStan summary.}
\item{pars}{Selected parameter names.}
\item{funs}{Quoted names of summary functions. `q_025` is accepted as
shorthand for `function(x) quantile(x, .025)`, and similarly `q_975`.}
\item{keep.rownames}{Whether to retain original parameter names with numeric
indexes, as output from RStan.}
}

\description{
All \code{\link{rstan}} methods for superclass
\code{\link[rstan]{stanfit-class}} are available. Descriptive labels for
parameters on time periods, local geographic areas, and grouping variables
will be added to most output.
}

\section{Slots}{
\describe{
\item{\code{dgirt_in}}{\code{\link{dgirtin-class}} data used to fit the model.}
}
}

\examples{
data(toy_dgirtfit)

# summarize the fitted results
summary(toy_dgirtfit)

# alternatively get the more verbose summary() output from RStan
summary(toy_dgirtfit, pars = "theta_bar", verbose = TRUE)

# get posterior means
get_posterior_mean(toy_dgirtfit)

# access the posterior samples
as.data.frame(toy_dgirtfit, pars = 'theta_bar')

# apply arbitrary functions to posterior samples
summarize(toy_dgirtfit, funs = "mean")
}

\seealso{
  \code{\link{stanfit-class}}, \code{\link{dgirtin-class}}
}
