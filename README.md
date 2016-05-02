[![Build Status](https://travis-ci.org/jamesdunham/dgirt.svg?branch=master)](https://travis-ci.org/jamesdunham/dgirt)

dgirt is an R package for dynamic group-level IRT models as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html):

> Over the past eight decades, millions of people have been surveyed on their political opinions. Until recently, however, polls rarely included enough questions in a given domain to apply scaling techniques such as IRT models at the individual level, preventing scholars from taking full advantage of historical survey data. To address this problem, we develop a Bayesian group-level IRT approach that models latent traits at the level of demographic and/or geographic groups rather than individuals. We use a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units. This framework opens up vast new areas of research on historical public opinion, especially at the subnational level.

Quick start
===========

Install from GitHub. `dgirt` requires [RStan](https://github.com/stan-dev/rstan). See the RStan [installation instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-install-rstan) for help.

    devtools::install_github("jamesdunham/dgirt", dependencies = TRUE)

Shape item response data for modeling:

``` r
library(dgirt)
#> Loading required package: Rcpp
#> Loading required package: rstan
#> Loading required package: ggplot2
#> rstan (Version 2.9.0-3, packaged: 2016-02-11 15:54:41 UTC, GitRev: 05c3d0058b6a)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> rstan_options(auto_write = TRUE)
#> options(mc.cores = parallel::detectCores())
dgirt_in <- shape(opinion, item_names = "Q_cces2006_abortion",
                  time_name = "year", geo_name = "state", group_names = "race",
                  time_filter = 2006:2008, geo_filter = c("MA", "NY"),
                  survey_name = "source", weight_name = "weight")
#> Applying restrictions, pass 1...
#>  Dropped 5 rows for missingness in covariates
#>  Dropped 215 rows for lacking item responses
#> Applying restrictions, pass 2...
#>  No changes
#> 
#> item                               class      levels       responses
#> --------------------------------------------------------------------
#> Q_cces2006_abortion              integer           2           5,313
#> --------------------------------------------------------------------
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
#> [1] "stateNY"   "raceother" "racewhite"
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
dgirt_out <- dgirt(dgirt_in, iter = 2000, chains = 2, cores = 2, seed = 42)
# verbose output omitted
```

All `rstan` methods for the `stanfit` class are available for `dgirt` output, so [do things the RStan way](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-use-rstan). Descriptive labels for parameters on time periods, local geographic areas, and grouping variables will be added to most output.

Summarize the results:

``` r
summary(dgirt_out, pars = "theta_bar", probs = NULL)[[1]][, c("n_eff", "Rhat")]
#>                              n_eff     Rhat
#> theta_bar[MA__black,2006] 617.8521 1.002874
#> theta_bar[NY__other,2006] 550.4068 1.004465
#> theta_bar[MA__black,2006] 648.2662 1.004901
#> theta_bar[NY__other,2006] 493.8424 1.005667
#> theta_bar[MA__black,2006] 597.2855 1.003263
#> theta_bar[NY__other,2006] 602.2568 1.003630
#> theta_bar[NY__black,2007] 161.0472 1.007891
#> theta_bar[MA__white,2007] 159.8824 1.010316
#> theta_bar[NY__black,2007] 165.9396 1.008898
#> theta_bar[MA__white,2007] 186.6746 1.007097
#> theta_bar[NY__black,2007] 161.5272 1.010481
#> theta_bar[MA__white,2007] 159.2225 1.009206
#> theta_bar[MA__other,2008] 130.0661 1.003498
#> theta_bar[NY__white,2008] 122.5863 1.004336
#> theta_bar[MA__other,2008] 130.9246 1.003504
#> theta_bar[NY__white,2008] 126.0247 1.003744
#> theta_bar[MA__other,2008] 120.6686 1.004224
#> theta_bar[NY__white,2008] 121.6400 1.004121
```

Get posterior means with a convenience function:

``` r
get_posterior_mean(dgirt_out, pars = "theta_bar")
#>                           mean-chain:1 mean-chain:2 mean-all chains
#> theta_bar[MA__black,2006]    11.150921    12.016116       11.583519
#> theta_bar[NY__black,2007]    11.037050    11.975606       11.506328
#> theta_bar[MA__other,2008]     8.869601     9.694063        9.281832
#> theta_bar[NY__other,2006]     6.527898     7.141562        6.834730
#> theta_bar[MA__white,2007]     8.724813     9.407850        9.066331
#> theta_bar[NY__white,2008]     7.022367     7.585582        7.303974
#> theta_bar[MA__black,2006]    30.322840    35.816811       33.069826
#> theta_bar[NY__black,2007]    28.529103    33.990859       31.259981
#> theta_bar[MA__other,2008]    23.195351    27.176713       25.186032
#> theta_bar[NY__other,2006]    15.940862    18.621480       17.281171
#> theta_bar[MA__white,2007]    24.566486    29.207524       26.887005
#> theta_bar[NY__white,2008]    17.802850    20.987815       19.395333
#> theta_bar[MA__black,2006]   131.893221   203.277472      167.585347
#> theta_bar[NY__black,2007]   127.041911   202.530328      164.786119
#> theta_bar[MA__other,2008]    97.761362   151.816903      124.789133
#> theta_bar[NY__other,2006]    73.252799   107.939084       90.595942
#> theta_bar[MA__white,2007]   103.772838   163.383290      133.578064
#> theta_bar[NY__white,2008]    83.912056   129.516051      106.714053
```

Or generally access posterior samples:

``` r
apply(as.array(dgirt_out, pars = "theta_bar"), 3, mean)
#> theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
#>                 11.583519                 33.069826 
#> theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
#>                167.585347                 11.506328 
#> theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
#>                 31.259981                164.786119 
#> theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
#>                  9.281832                 25.186032 
#> theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
#>                124.789133                  6.834730 
#> theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
#>                 17.281171                 90.595942 
#> theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
#>                  9.066331                 26.887005 
#> theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
#>                133.578064                  7.303974 
#> theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
#>                 19.395333                106.714053
```

See `help("dgirtfit-class")` and `help("stanfit-class")` for more.
