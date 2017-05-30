
[![Build Status](https://travis-ci.org/jamesdunham/dgo.svg?branch=master)](https://travis-ci.org/jamesdunham/dgo) [![Build status](https://ci.appveyor.com/api/projects/status/1ta36kmoqen98k87?svg=true)](https://ci.appveyor.com/project/jamesdunham/dgo) [![codecov](https://codecov.io/gh/jamesdunham/dgo/branch/master/graph/badge.svg)](https://codecov.io/gh/jamesdunham/dgo)

Introduction
============

dgo is an R package for the dynamic estimation of group-level opinion. The package can be used to estimate subpopulation groups' average latent conservatism (or other latent trait) from individuals' responses to dichotomous questions using a Bayesian group-level IRT approach developed by [Caughey and Warshaw 2015](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html) that models latent traits at the level of demographic and/or geographic groups rather than individuals. This approach uses a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units, such as states.

dgo can also be used to estimate smoothed estimates of subpopulation groups' average responses on individual survey questions using a dynamic multi-level regression and poststratification (MRP) model ([Park, Gelman, and Bafumi 2004](http://stat.columbia.edu/~gelman/research/published/StateOpinionsNationalPolls.050712.dkp.pdf)). For instance, it could be used to estimate public opinion in each state on same-sex marriage or the Affordable Care Act.

This model opens up new areas of research on historical public opinion in the United States at the subnational level. It also enables scholars of comparative politics to estimate dynamic models of public opinion opinion at the country or subnational level.

Installation
============

dgo can be installed from [CRAN](https://cran.r-project.org/web/packages/dgo/index.html):

``` r
install.packages("dgo")
```

Or get the latest version from [GitHub](https://github.com/jamesdunham/dgo) using [devtools](https://github.com/hadley/devtools/):

``` r
if (!require(devtools, quietly = TRUE)) install.packages("devtools")
devtools::install_github("jamesdunham/dgo")
```

dgo requires a working installation of [RStan](http://mc-stan.org/interfaces/rstan.html). If you don't have already have RStan, follow its "[Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)" guide.

Usage
=====

Load the package and set RStan's recommended options for a local, multicore machine with excess RAM:

``` r
library(dgo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

The minimal workflow from raw data to estimation is:

1.  shape input data using the `shape()` function; and
2.  pass the result to the `dgirt()` function to estimate a latent trait (e.g., conservatism) or `dgmrp()` function to estimate opinion on a single survey question.

Troubleshooting
===============

Please [report issues](https://github.com/jamesdunham/dgo/issues) that you encounter.

-   OS X only: RStan creates temporary files during estimation in a location given by `tempdir()`, typically an arbitrary location in `/var/folders`. If a model runs for days, these files can be cleaned up while still needed, which induces an error. A good solution is to set a safer path for temporary files, using an environment variable checked at session startup. For help setting environment variables, see the Stack Overflow question [here](https://stackoverflow.com/questions/17107206/change-temporary-directory). Confirm the new path before starting your model run by restarting R and checking the output from `tempdir()`.

-   Models fitted before October 2016 (specifically &lt; [\#8e6a2cf](https://github.com/jamesdunham/dgo/commit/8e6a2cfbe00b2cd4a908b3067241e06124d143cd)) using dgirt are not fully compatible with dgo. Their contents can be extracted without using dgo, however, with the `$` indexing operator. For example: `as.data.frame(dgirtfit_object$stan.cmb)`.

-   Calling `dgirt()` or `dgmrp()` can generate [warnings](http://mc-stan.org/misc/warnings#compiler-warnings) during model compilation. These are safe to ignore, or can be suppressed by following the linked instructions.

Contributing and citing
=======================

dgo is under development and we welcome [suggestions](https://github.com/jamesdunham/dgo/issues).

The package citation is:

Dunham, James, Devin Caughey, and Christopher Warshaw. 2017. dgo: Dynamic Estimation of Group-level Opinion. R package. <https://jdunham.io/dgo/>.
