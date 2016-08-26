[![Build Status](https://travis-ci.org/jamesdunham/dgirt.svg?branch=master)](https://travis-ci.org/jamesdunham/dgirt)

dgirt is an R package for dynamic group-level item response theory (DGIRT) models. The DGIRT model is a Bayesian method for estimating subpopulation groups' average conservatism (or other trait) from individuals' responses to dichotomous questions. It is "dynamic" both in the sense that groups are allowed to evolve over time and in the sense that the model "borrows strength" from other time periods, to a degree specified by the user. [This document](https://github.com/jamesdunham/dgirt/blob/master/man/dgirt_details.pdf) describes the model in detail.

It is a modified version of the hierarchical group-level IRT model implemented by [Caughey and Warshaw 2015](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html).

Prerequisites
-------------

Installation requires [RStan](http://mc-stan.org/interfaces/rstan.html) and its prerequisites, in particular a C++ toolchain. If you don't have RStan, follow its "[Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)" guide before continuing.

Installation
------------

dgirt can be installed from [GitHub](https://github.com/jamesdunham/dgirt) using [devtools](https://github.com/hadley/devtools/):

    devtools::install_github("jamesdunham/dgirt", dependencies = TRUE)

Getting started
---------------

``` r
library(dgirt)
```

The minimal workflow from raw data to estimation is:

1.  shape input data using the `shape` function; and
2.  pass the result to the `dgirt` function to fit a dgirt model.

### Set RStan options

These are RStan's recommended options on a local, multicore machine with excess RAM:

``` r
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

### Prepare input data with `shape`

dgirt models are *dynamic*, so we need to specify which variable in the data represents time. They are also *group-level*, with groups defined by one variable for respondents' local geographic area and one or more variables for respondent characteristics.

The `time_filter` and `geo_filter` arguments optionally subset the data. Finally, `shape` requires the names of the survey identifier and survey weight variables in the data.

``` r
dgirt_in <- shape(opinion,
                  item_names = "Q_cces2006_abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "race",
                  geo_filter = c("CA", "GA", "LA", "MA"),
                  survey_name = "source",
                  weight_name = "weight")
#> Applying restrictions, pass 1...
#>  Dropped 5 rows for missingness in covariates
#>  Dropped 555 rows for lacking item responses
#> Applying restrictions, pass 2...
#>  No changes
```

The reshaped and subsetted data can be summarized in a few ways before model fitting.

``` r
summary(dgirt_in)
#> Items:
#> [1] "Q_cces2006_abortion"
#> Respondents:
#>    11,662 in `item_data` (unadjusted)
#> Grouping variables:
#> [1] "year"  "state" "race" 
#> Time periods:
#> [1] 2006 2007 2008
#> Local geographic areas:
#> [1] "CA" "GA" "LA" "MA"
#> Hierarchical parameters:
#> [1] "GA"        "LA"        "MA"        "raceother" "racewhite"
#> Modifiers of hierarchical parameters:
#> character(0)
#> Constants:
#>  Q  T  P  N  G  H  D 
#>  1  3  5 36 12  1  1
```

Response counts by survey-year:

``` r
get_n(dgirt_in, by = c("year", "source"))
#>    year    source    n
#> 1: 2006 CCES_2006 5275
#> 2: 2007 CCES_2007 1690
#> 3: 2008 CCES_2008 4697
```

Response counts by item-year:

``` r
get_item_n(dgirt_in, by = "year")
#>    year Q_cces2006_abortion
#> 1: 2006                5275
#> 2: 2007                1690
#> 3: 2008                4697
```

### Fit a model with `dgirt`

`dgirt` fits a model to data from `shape`. Under the hood, this function uses RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out <- dgirt(dgirt_in, iter = 1500, chains = 4, cores = 4, seed = 42,
                   refresh = 0)
```

The model results are held in a `dgirtfit` object. Methods from RStan like `extract` are available if needed because `dgirtfit` is a subclass of `stanfit`. But dgirt provides its own methods for typical post-estimation tasks.

### Work with `dgirt` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out)
#> dgirt samples from 4 chains of 1500 iterations, 750 warmup, thinned every 1 
#>   Drawn Fri Aug 26 08:19:58 2016 
#>   Package version not available (< 0.2.2) 
#>   Model version 2016_08_11 
#>   86 parameters; 36 theta_bars (year, state and race)
#>   3 periods 2006 to 2008 
#> 
#> n_eff
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   15.55  315.70  536.30  856.80 1148.00 3000.00 
#> 
#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.9987  1.0020  1.0060  1.0070  1.0100  1.0260       1
```

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
head(summarize(dgirt_out))
#>        param state  race year       mean         sd    median    q_025
#> 1: theta_bar    CA black 2006   9.800682   5.075017  8.950097 2.859070
#> 2: theta_bar    CA black 2007  29.522746  24.099714 23.172774 6.725454
#> 3: theta_bar    CA black 2008 149.768144 346.296790 70.781306 7.521626
#> 4: theta_bar    CA other 2006   7.221263   3.845966  6.498232 2.050227
#> 5: theta_bar    CA other 2007  17.289936  15.439826 13.028520 3.030528
#> 6: theta_bar    CA other 2008  81.398526 212.647094 34.528919 2.935586
#>        q_975
#> 1:  21.96886
#> 2:  90.42006
#> 3: 702.67306
#> 4:  16.54213
#> 5:  61.22678
#> 6: 404.08954
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out, pars = "xi", funs = "var")
#>    param year      var
#> 1:    xi 2006 24.16381
#> 2:    xi 2007 27.57924
#> 3:    xi 2008 60.28344
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
head(as.data.frame(dgirt_out))
#>        param state  race year iteration     value
#> 1: theta_bar    CA black 2006         1  8.754243
#> 2: theta_bar    CA black 2006         2  5.175876
#> 3: theta_bar    CA black 2006         3 14.378213
#> 4: theta_bar    CA black 2006         4 17.340792
#> 5: theta_bar    CA black 2006         5 19.528374
#> 6: theta_bar    CA black 2006         6 17.078189
```

To poststratify the results use `poststratify`. The following example uses the group population proportions bundled as `state_year_targets` to reweight and aggregate estimates to strata defined by state-years. Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out, state_year_targets, strata_names = c("state", "year"),
             aggregated_names = "race")
#>     state year       value
#>  1:    CA 2006   7.3515956
#>  2:    CA 2007  21.2947038
#>  3:    CA 2008 102.6553194
#>  4:    GA 2006   4.0615232
#>  5:    GA 2007   7.8487587
#>  6:    GA 2008  22.7679818
#>  7:    LA 2006  -0.4403615
#>  8:    LA 2007  -7.5520691
#>  9:    LA 2008 -57.0560250
#> 10:    MA 2006   9.4479394
#> 11:    MA 2007  33.7456582
#> 12:    MA 2008 159.0233212
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area. For this (unconverged) toy example we omit the CIs.

``` r
dgirt_plot(dgirt_out, y_min = NULL, y_max = NULL)
```

![](README-unnamed-chunk-13-1.png)

Output from `dgirt_plot` can be customized to some extent using objects from the ggplot2 package.

``` r
dgirt_plot(dgirt_out, y_min = NULL, y_max = NULL) + theme_classic()
```

![](README-unnamed-chunk-14-1.png)

`dgirt_plot` can also plot the `data.frame` output from `poststratify`. This requires arguments that identify the relevant variables in the `data.frame`. Below, `poststratify` aggregates over the demographic grouping variable `race`, resulting in a `data.frame` of estimates by state-year. So, in the subsequent call to `dgirt_plot`, we pass the names of the state and year variables. The `group_names` argument is `NULL` because there are no grouping variables left after aggregating over `race`.

``` r
ps <- poststratify(dgirt_out, state_year_targets,
                   strata_names = c("state", "year"), aggregated_names = "race")
head(ps)
#>    state year      value
#> 1:    CA 2006   7.351596
#> 2:    CA 2007  21.294704
#> 3:    CA 2008 102.655319
#> 4:    GA 2006   4.061523
#> 5:    GA 2007   7.848759
#> 6:    GA 2008  22.767982
dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
```

![](README-unnamed-chunk-15-1.png)

Troubleshooting
---------------

Please [report issues](https://github.com/jamesdunham/dgirt/issues) that you encounter.

OS X only: RStan creates temporary files during estimation in a location given by `tempdir`, typically an arbitrary location in `/var/folders`. If a model runs for days, these files can be cleaned up while still needed, which induces an error. A good solution is to set a safer path for temporary files, using an environment variable checked at session startup. As described in `?tempdir`,

> The environment variables ‘TMPDIR’, ‘TMP’ and ‘TEMP’ are checked in turn and the first found which points to a writable directory is used: if none succeeds ‘/tmp’ is used. The path should not contain spaces.

For help setting environment variables, see the Stack Overflow question [here](https://stackoverflow.com/questions/17107206/change-temporary-directory). Confirm the new path before starting your model run by restarting R and checking the output from `tempdir()`.

``` r
# Problematic temporary directories on OS X look like this
tempdir()   
#> [1] "/var/folders/2p/_d3c95qd6ljg28j1f5l2jqxm0000gn/T//Rtmp38a10A"
```

Contributing and citing
-----------------------

dgirt is under development and we welcome [suggestions](https://github.com/jamesdunham/dgirt/issues).

The package citation is

> Dunham, James, Devin Caughey, and Christopher Warshaw. 2016. dgirt: Dynamic Group-level IRT Models in R. R package. <https://jamesdunham. github.io/dgirt/>.
