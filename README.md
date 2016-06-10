[![Build Status](https://travis-ci.org/jamesdunham/dgirt.svg?branch=master)](https://travis-ci.org/jamesdunham/dgirt)

dgirt is an R package for dynamic group-level IRT models as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html).

Quick start
===========

Install from GitHub. dgirt requires [RStan](http://mc-stan.org/interfaces/rstan.html) and its [prerequisites](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#prerequisites).

    devtools::install_github("jamesdunham/dgirt", dependencies = TRUE)

RStan's recommended option settings on a local, multicore machine with excess RAM are equally appropriate for dgirt.

``` r
library(dgirt)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

The minimal workflow from raw data to estimation is to 1) shape input data using the `shape` function, and 2) pass the result to the `dgirt` function to fit a dgirt model.

For this example we'll model responses to a single survey item in the bundled `opinion` data. dgirt models are *dynamic*, so we need to specify which variable in the data represents time. They are also *group-level*, so we need to define the groups with one variable for respondents' local geographic area and one or more variables for respondent characteristics.

The `time_filter` and `geo_filter` arguments optionally subset the example data. `time_filter` may include unobserved periods.

Finally, `shape` requires the names of the survey identifier and survey weight variables in the data.

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

Fit a model to data from `shape` using `dgirt`. Under the hood, this function uses RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via `dgirt`'s `...` argument. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out <- dgirt(dgirt_in, iter = 2000, chains = 2, cores = 2, seed = 42,
                   refresh = 0)
```

The model results are held in a `dgirtfit` object. This means that methods from RStan like `extract` are available if needed. But dgirt provides methods for a typical workflow.

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out)
#> dgirt samples from 2 chains of 2000 iterations, 1000 warmup, thinned every 1 
#>   Drawn Fri Jun 10 11:33:25 2016 
#>   Package version not available (< 0.2.2) 
#>   Model version 2016_04_20 
#>   62 parameters; 18 theta_bars (year, state and race)
#>   3 periods 2006 to 2008 
#> 
#> n_eff
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    7.968  158.500  492.500  548.400  691.100 2000.000 
#> 
#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   0.999   1.002   1.004   1.004   1.006   1.011       1
```

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
summarize(dgirt_out)
#>         param state  race year       mean         sd    median    q_025      q_975
#>  1: theta_bar    MA black 2006  12.016116   6.180265 11.126649 2.890208   26.31390
#>  2: theta_bar    NY black 2006  11.975606   6.338594 10.899991 2.972557   27.31934
#>  3: theta_bar    MA other 2006   9.694063   5.383397  8.662351 2.159478   22.72634
#>  4: theta_bar    NY other 2006   7.141562   4.176749  6.273520 1.439215   16.74738
#>  5: theta_bar    MA white 2006   9.407850   4.995681  8.495221 2.253769   21.16692
#>  6: theta_bar    NY white 2006   7.585582   4.078906  6.831911 1.830558   16.85937
#>  7: theta_bar    MA black 2007  35.816811  35.393705 24.354100 5.898169  119.43292
#>  8: theta_bar    NY black 2007  33.990859  34.261889 22.753406 5.057470  127.61262
#>  9: theta_bar    MA other 2007  27.176713  27.959494 18.329223 3.483455   98.47461
#> 10: theta_bar    NY other 2007  18.621480  19.550618 12.284055 1.790950   71.61944
#> 11: theta_bar    MA white 2007  29.207524  27.540693 20.591757 4.934922  104.23598
#> 12: theta_bar    NY white 2007  20.987815  21.224853 14.266239 3.105101   77.14955
#> 13: theta_bar    MA black 2008 203.277472 592.000212 43.760531 3.798287 1536.14980
#> 14: theta_bar    NY black 2008 202.530328 582.478001 42.888168 4.575970 1634.73428
#> 15: theta_bar    MA other 2008 151.816903 482.499304 28.833956 1.405715 1232.46593
#> 16: theta_bar    NY other 2008 107.939084 292.954853 23.748811 1.845565  987.12463
#> 17: theta_bar    MA white 2008 163.383290 462.012933 36.394111 3.945969 1397.93728
#> 18: theta_bar    NY white 2008 129.516051 359.494356 30.339634 3.355217 1048.16265
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Function names should be quoted. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out, pars = "xi", funs = "var")
#>    param year      var
#> 1:    xi 2006 28.15121
#> 2:    xi 2007 47.83046
#> 3:    xi 2008 79.63748
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like most other methods takes a `pars` argument.

``` r
as.data.frame(dgirt_out)
#>            param state  race year iteration     value
#>     1: theta_bar    MA black 2006      1001 12.713996
#>     2: theta_bar    NY black 2006      1001 11.806746
#>     3: theta_bar    MA other 2006      1001  7.433034
#>     4: theta_bar    NY other 2006      1001  7.002287
#>     5: theta_bar    MA white 2006      1001  8.816428
#>    ---                                               
#> 17996: theta_bar    NY black 2008      2000 66.130567
#> 17997: theta_bar    MA other 2008      2000 26.497293
#> 17998: theta_bar    NY other 2008      2000 14.452522
#> 17999: theta_bar    MA white 2008      2000 47.693500
#> 18000: theta_bar    NY white 2008      2000 30.575843
```

To poststratify the results use `poststratify`. More details are given in `help("poststratify")`. This example uses the group population proportions bundled as `targets` to reweight and aggregate estimates to strata defined by states and years.

``` r
# poststratify(dgirt_out, targets, c("state", "year"), "race")
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area.

``` r
# dgirt_plot(dgirt_out)
```

Output from `dgirt_plot` can be customized using objects from the ggplot2 package. Chain them to the `dgirt_plot` call with `+` or use the `%+%` operator with existing `ggplot` objects.

``` r
# dgirt_plot(dgirt_out) + ylab("median")

# p <- dgirt_plot(dgirt_out)
# p <- p %+% ylab("median")
```
