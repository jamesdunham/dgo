dgo: Dynamic Estimation of Group-Level Opinion
================

[![Build
Status](https://travis-ci.org/jamesdunham/dgo.svg?branch=master)](https://travis-ci.org/jamesdunham/dgo)
[![Build
status](https://ci.appveyor.com/api/projects/status/1ta36kmoqen98k87?svg=true)](https://ci.appveyor.com/project/jamesdunham/dgo)
[![codecov](https://codecov.io/gh/jamesdunham/dgo/branch/master/graph/badge.svg)](https://codecov.io/gh/jamesdunham/dgo)

# Introduction

dgo is an R package for the dynamic estimation of group-level public
opinion. You can use the package to estimate latent trait means in
subpopulations from survey data. For example, dgo can estimate the
average policy liberalism in each American state over time among
Democrats, Independents, and Republicans, given their answers to survey
questions about policy proposals.

dgo accomplishes this using a Bayesian group-level IRT approach
developed by [Caughey and Warshaw
2015](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html).
It models latent traits at the level of demographic and geographic
groups rather than individuals. It uses a hierarchical model to borrow
strength cross-sectionally and dynamic linear models to do so across
time.

The package can also be used to estimate smoothed estimates of
subpopulations’ average responses to single survey items, using a
dynamic multi-level regression and poststratification (MRP) model
([Park, Gelman, and Bafumi
2004](http://stat.columbia.edu/~gelman/research/published/StateOpinionsNationalPolls.050712.dkp.pdf)).
For instance, you can use dgo to estimate public opinion in each state
on same-sex marriage or the Affordable Care Act.

This model opens up new areas of research on historical public opinion
in the United States at the subnational level. It also allows scholars
of comparative politics to estimate dynamic cross-national models of
public opinion.

# Installation

dgo can be installed from
[CRAN](https://CRAN.R-project.org/package=dgo):

``` r
install.packages("dgo")
```

Or get the latest version from
[GitHub](https://github.com/jamesdunham/dgo) using
[devtools](https://github.com/hadley/devtools/):

``` r
if (!require(devtools, quietly = TRUE)) install.packages("devtools")
devtools::install_github("jamesdunham/dgo")
```

dgo requires a working installation of
[RStan](http://mc-stan.org/interfaces/rstan.html). If you don’t have
already have RStan, follow its “[Getting
Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)”
guide.

# Usage

Load the package and set RStan’s recommended options for a local,
multicore machine with excess RAM:

``` r
library(dgo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

The minimal workflow from raw data to estimation is:

1.  shape input data using the `shape()` function; and
2.  pass the result to the `dgirt()` function to estimate a latent trait
    (e.g., conservatism) or `dgmrp()` function to estimate opinion on a
    single survey question.

# Troubleshooting

Please [report issues](https://github.com/jamesdunham/dgo/issues) that
you encounter.

  - OS X only: RStan creates temporary files during estimation in a
    location given by `tempdir()`, typically an arbitrary location in
    `/var/folders`. If a model runs for days, these files can be cleaned
    up while still needed, which induces an error. A good solution is to
    set a safer path for temporary files, using an environment variable
    checked at session startup. For help setting environment variables,
    see the Stack Overflow question
    [here](https://stackoverflow.com/questions/17107206/change-temporary-directory).
    Confirm the new path before starting your model run by restarting R
    and checking the output from `tempdir()`.

  - Models fitted before October 2016 (specifically \<
    [\#8e6a2cf](https://github.com/jamesdunham/dgo/commit/8e6a2cfbe00b2cd4a908b3067241e06124d143cd))
    using dgirt are not fully compatible with dgo. Their contents can be
    extracted without using dgo, however, with the `$` indexing
    operator. For example: `as.data.frame(dgirtfit_object$stan.cmb)`.

  - Calling `dgirt()` or `dgmrp()` can generate
    [warnings](http://mc-stan.org/misc/warnings#compiler-warnings)
    during model compilation. These are safe to ignore, or can be
    suppressed by following the linked instructions.

# Contributing and citing

dgo is under development and we welcome
[suggestions](https://github.com/jamesdunham/dgo/issues).

The package citation is:

Dunham, James, Devin Caughey, and Christopher Warshaw. 2018. dgo:
Dynamic Estimation of Group-level Opinion. R package.
<https://jdunham.io/dgo/>.
