[![Build Status](https://travis-ci.org/jamesdunham/dgirt.svg?branch=master)](https://travis-ci.org/jamesdunham/dgirt)

dgirt is an R package for dynamic group-level IRT models as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html).

Quick start
===========

Install from GitHub. dgirt requires [RStan](http://mc-stan.org/interfaces/rstan.html) and its [prerequisites](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#prerequisites).

    devtools::install_github("jamesdunham/dgirt", dependencies = TRUE)

Shape item response data for modeling:

``` r
library(dgirt)
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

Summarize the data:

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
```

Get item response counts:

``` r
get_item_n(dgirt_in, by = "year")
#>    year Q_cces2006_abortion
#> 1: 2006                2142
#> 2: 2007                 838
#> 3: 2008                2333
```

Or response counts generally:

``` r
get_n(dgirt_in, by = c("year", "source"))
#>    year    source    n
#> 1: 2006 CCES_2006 2142
#> 2: 2007 CCES_2007  838
#> 3: 2008 CCES_2008 2333
```

Fit a DGIRT model:

``` r
dgirt_out <- dgirt(dgirt_in, iter = 2000, chains = 2, cores = 2, seed = 42,
                   refresh = 0)
```

All `rstan` methods for the `stanfit` class are available for `dgirt` output, so [do things the RStan way](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-use-rstan). Descriptive labels for parameters on time periods, local geographic areas, and grouping variables will be added to most output.

Summarize the results:

``` r
summary(dgirt_out)
#> dgirt samples from 2 chains of 2000 iterations, 1000 warmup, thinned every 1 
#>   Drawn Thu Jun  9 11:23:27 2016 
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

Get posterior means with a convenience function, by default for the group means `theta_bar`:

``` r
get_posterior_mean(dgirt_out, pars = "theta_bar")
#>         param state  race year       mean
#>  1: theta_bar    MA black 2006  12.016116
#>  2: theta_bar    NY black 2006  11.975606
#>  3: theta_bar    MA other 2006   9.694063
#>  4: theta_bar    NY other 2006   7.141562
#>  5: theta_bar    MA white 2006   9.407850
#>  6: theta_bar    NY white 2006   7.585582
#>  7: theta_bar    MA black 2007  35.816811
#>  8: theta_bar    NY black 2007  33.990859
#>  9: theta_bar    MA other 2007  27.176713
#> 10: theta_bar    NY other 2007  18.621480
#> 11: theta_bar    MA white 2007  29.207524
#> 12: theta_bar    NY white 2007  20.987815
#> 13: theta_bar    MA black 2008 203.277472
#> 14: theta_bar    NY black 2008 202.530328
#> 15: theta_bar    MA other 2008 151.816903
#> 16: theta_bar    NY other 2008 107.939084
#> 17: theta_bar    MA white 2008 163.383290
#> 18: theta_bar    NY white 2008 129.516051
```

Or generally access posterior samples, by default for the group means `theta_bar`:

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

See `help("dgirtfit-class")` and `help("stanfit-class")` for more.
