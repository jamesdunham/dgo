\name{dgirt}
\alias{dgirt}
\title{\code{dgirt}: fit a DGIRT model}
\usage{
dgirt(shaped_data, ..., separate_t = FALSE, delta_tbar_prior_mean = 0.5,
  delta_tbar_prior_sd = 0.5, innov_sd_delta_scale = 2.5,
  innov_sd_theta_scale = 2.5, version = "2016_09_14")
}
\arguments{
\item{shaped_data}{Output from \code{\link{shape}}.}

\item{...}{Further arguments passed to \code{\link[rstan]{stan}}.}

\item{separate_t}{Whether smoothing of estimates over time should be
disabled. Default \code{FALSE}.}

\item{delta_tbar_prior_mean}{Prior mean for \code{delta_tbar}, the normal
weight on \code{theta_bar} in the previous period.  Default \code{0.5}.}

\item{delta_tbar_prior_sd}{Prior standard deviation for \code{delta_bar}.
Default \code{0.5}.}

\item{innov_sd_delta_scale}{Prior scale for \code{sd_innov_delta}, the Cauchy
innovation standard deviation of \code{nu_geo} and \code{delta_gamma}.
Default \code{2.5}.}

\item{innov_sd_theta_scale}{Prior scale for \code{sd_innov_theta}, the Cauchy
innovation standard deviation of \code{gamma}, \code{xi}, and if
\code{constant_item} is \code{FALSE} the item difficulty \code{diff}. Default
\code{2.5}.}

\item{version}{The version of the DGIRT model to use. Use \code{link{models}}
  to see available versions.}
}
\value{
A \code{\link{dgirtfit-class}} object that extends
\code{\link[rstan]{stanfit-class}}.
}
\description{
\code{dgirt} makes a call to \code{\link[rstan]{stan}} with the Stan code and
data for a DGIRT model. 

The user will typically pass further arguments to \code{\link[rstan]{stan}}
via the \code{...} argument, at a minimum \code{iters} and \code{cores}.

By default \code{dgirt} overrides the \code{\link[rstan]{stan}} default for its
\code{pars} argument to specify typical DGIRT parameters of interest.
\code{dgirt} also sets \code{iter_r} to \code{1L}. 
}
\seealso{
\code{dgirt} expects \code{shaped_data} created by \code{\link{shape}} and
returns an object of class \code{\link{dgirtfit-class}}.
}

