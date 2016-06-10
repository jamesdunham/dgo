[![Build Status](https://travis-ci.org/jamesdunham/dgirt.svg?branch=master)](https://travis-ci.org/jamesdunham/dgirt)

dgirt is an R package for dynamic group-level IRT models as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html).

Install
-------

Install dgirt from GitHub. Installation requires [RStan](http://mc-stan.org/interfaces/rstan.html) and its prerequisites, in particular a C++ toolchain. If you don't have RStan, follow its "[Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)" guide.

    devtools::install_github("jamesdunham/dgirt", dependencies = TRUE)

RStan's recommended options on a local, multicore machine with excess RAM are also appropriate for dgirt:

``` r
library(dgirt)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

Getting started
---------------

The minimal workflow from raw data to estimation is:

1.  shape input data using the `shape` function; and
2.  pass the result to the `dgirt` function to fit a dgirt model.

### Prepare input data with `shape`

The following example models responses to a single survey item in the bundled `opinion` data. dgirt models are *dynamic*, so we need to specify which variable in the data represents time. They are also *group-level*, with groups defined by one variable for respondents' local geographic area and one or more variables for respondent characteristics.

The `time_filter` and `geo_filter` arguments optionally subset the example data. `time_filter` may include unobserved periods. Finally, `shape` requires the names of the survey identifier and survey weight variables in the data.

``` r
dgirt_in <- shape(opinion, item_names = "Q_cces2006_abortion",
                  time_name = "year", geo_name = "state", group_names = "race",
                  time_filter = 2006:2008, geo_filter = c("MA", "NY"),
                  survey_name = "source", weight_name = "weight")
#> Applying restrictions, pass 1...
#>  Dropped 5 rows for missingness in covariates
#>  Dropped 215 rows for lacking item responses
#> Applying restrictions, pass 2...
#>  No changes
```

The reshaped and subsetted data can be summarized in a few ways before model fitting.

``` r
summary(dgirt_in)
#> Items:
#> [1] "Q_cces2006_abortion"
#> Respondents:
#>    5,313 in `item_data` (unadjusted)
#> Grouping variables:
#> [1] "year"  "state" "race" 
#> Time periods:
#> [1] 2006 2007 2008
#> Local geographic areas:
#> [1] "MA" "NY"
#> Hierarchical parameters:
#> [1] "NY"        "raceother" "racewhite"
#> Modifiers of hierarchical parameters:
#> character(0)
#> Constants:
#>  Q  T  P  N  G  H  D 
#>  1  3  3 18  6  1  1
get_n(dgirt_in, by = c("year", "source"))
#>    year    source    n
#> 1: 2006 CCES_2006 2142
#> 2: 2007 CCES_2007  838
#> 3: 2008 CCES_2008 2333
get_item_n(dgirt_in, by = "year")
#>    year Q_cces2006_abortion
#> 1: 2006                2142
#> 2: 2007                 838
#> 3: 2008                2333
```

### Fit a model with `dgirt`

We use `dgirt` to fit a model to data from `shape`. Under the hood, this function uses RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out <- dgirt(dgirt_in, iter = 2000, chains = 4, cores = 4, seed = 42,
                   refresh = 0)
```

The model results are held in a `dgirtfit` object. This means that methods from RStan like `extract` are available if needed. But dgirt provides its own methods for typical post-estimation tasks.

### Work with `dgirt` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out)
#> dgirt samples from 4 chains of 2000 iterations, 1000 warmup, thinned every 1 
#>   Drawn Fri Jun 10 16:24:04 2016 
#>   Package version not available (< 0.2.2) 
#>   Model version 2016_04_20 
#>   62 parameters; 18 theta_bars (year, state and race)
#>   3 periods 2006 to 2008 
#> 
#> n_eff
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   14.38   37.82  124.80  721.90  623.80 4000.00 
#> 
#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   0.999   1.010   1.032   1.050   1.089   1.232       1
```

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
summarize(dgirt_out)
#>         param state  race year        mean           sd    median
#>  1: theta_bar    MA black 2006   12.449113     6.055190 11.909883
#>  2: theta_bar    MA black 2007   79.058536   260.892896 23.177396
#>  3: theta_bar    MA black 2008 5668.696015 40492.137380 39.219760
#>  4: theta_bar    MA other 2006   10.669374     5.859043  9.551784
#>  5: theta_bar    MA other 2007   59.909209   202.181234 18.036069
#>  6: theta_bar    MA other 2008 4228.299816 31009.969421 27.678540
#>  7: theta_bar    MA white 2006    9.589761     4.681945  9.183307
#>  8: theta_bar    MA white 2007   62.900108   209.124274 19.877337
#>  9: theta_bar    MA white 2008 4450.244160 32179.562549 33.984820
#> 10: theta_bar    NY black 2006   12.121666     5.852140 11.794575
#> 11: theta_bar    NY black 2007   75.752930   254.718722 22.029504
#> 12: theta_bar    NY black 2008 5424.751843 38828.167769 40.174817
#> 13: theta_bar    NY other 2006    8.022173     4.834066  6.844107
#> 14: theta_bar    NY other 2007   41.276998   139.640491 12.159830
#> 15: theta_bar    NY other 2008 2950.896790 21681.800348 22.732268
#> 16: theta_bar    NY white 2006    7.681672     3.781180  7.381183
#> 17: theta_bar    NY white 2007   47.745653   164.049799 13.742124
#> 18: theta_bar    NY white 2008 3492.276208 25326.348861 27.954231
#>         q_025       q_975
#>  1: 3.2653429    25.79341
#>  2: 6.7036926   615.46435
#>  3: 5.1646778 53631.12411
#>  4: 2.3072615    22.34003
#>  5: 4.3012346   433.68271
#>  6: 0.8406976 38720.07454
#>  7: 2.5354057    20.49129
#>  8: 5.5851192   473.78587
#>  9: 4.8441186 41881.24096
#> 10: 3.2168802    25.96399
#> 11: 5.9078648   598.91648
#> 12: 6.0050650 50662.80251
#> 13: 1.5825031    16.86198
#> 14: 2.1226355   317.10045
#> 15: 1.6278213 26690.10411
#> 16: 1.9409956    16.44019
#> 17: 3.4517385   387.83017
#> 18: 4.2787013 32128.21029
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out, pars = "xi", funs = "var")
#>    param year      var
#> 1:    xi 2006 26.07239
#> 2:    xi 2007 49.92106
#> 3:    xi 2008 91.19223
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
as.data.frame(dgirt_out)
#>            param state  race year iteration    value
#>     1: theta_bar    MA black 2006      1001 12.71400
#>     2: theta_bar    MA black 2006      1002 26.00205
#>     3: theta_bar    MA black 2006      1003 32.48592
#>     4: theta_bar    MA black 2006      1004 12.93165
#>     5: theta_bar    MA black 2006      1005 13.86844
#>    ---                                              
#> 53996: theta_bar    NY white 2008      3996 38.24935
#> 53997: theta_bar    NY white 2008      3997 61.49403
#> 53998: theta_bar    NY white 2008      3998 29.21043
#> 53999: theta_bar    NY white 2008      3999 33.39294
#> 54000: theta_bar    NY white 2008      4000 22.07611
```

To poststratify the results use `poststratify`. This example uses the group population proportions bundled as `state_year_targets` to reweight and aggregate estimates to strata defined by state-years. Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out, state_year_targets, strata_names = c("state", "year"),
             aggregated_names = "race")
#>    state year       value
#> 1:    MA 2006    9.806214
#> 2:    MA 2007   63.495145
#> 3:    MA 2008 4496.516121
#> 4:    NY 2006    8.368551
#> 5:    NY 2007   51.077046
#> 6:    NY 2008 3710.533559
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area.

``` r
dgirt_plot(dgirt_out)
```

![](README-unnamed-chunk-10-1.png)

Output from `dgirt_plot` can be customized using objects from the ggplot2 package.

``` r
dgirt_plot(dgirt_out) + theme_classic()
```

![](README-unnamed-chunk-11-1.png)
