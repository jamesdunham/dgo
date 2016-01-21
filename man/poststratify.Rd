% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poststratify.r
\name{poststratify}
\alias{poststratify}
\title{Poststratify dgirt group means}
\usage{
poststratify(group_means, targets, strata = c("year", "state"), groups,
  prop_var = "proportion", check_proportions = NULL)
}
\arguments{
\item{group_means}{The `theta_bar` element of `dgirt` results; a data.frame.}

\item{targets}{Table in which rows correspond to the population
strata defined by the combinations of `strata` variables; a data.frame.}

\item{strata}{Variables in `targets` table that define population strata; a
character vector.}

\item{groups}{Variables in `group_means` table that give `dgirt` covariates;
a character vector.}

\item{prop_var}{Variable in `targets` table that gives the population
proportion of each stratum; a length-one character vector.}

\item{check_proportions}{Optionally, variables within whose combinations the
population proportions in `targets` should sum to one, otherwise an error
will appear; a character vector.}
}
\value{
table of poststratified group means
}
\description{
Poststratify dgirt group means
}
