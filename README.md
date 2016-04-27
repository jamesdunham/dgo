dgirt is an R package for dynamic group-level IRT models as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html):

> Over the past eight decades, millions of people have been surveyed on their political opinions. Until recently, however, polls rarely included enough questions in a given domain to apply scaling techniques such as IRT models at the individual level, preventing scholars from taking full advantage of historical survey data. To address this problem, we develop a Bayesian group-level IRT approach that models latent traits at the level of demographic and/or geographic groups rather than individuals. We use a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units. This framework opens up vast new areas of research on historical public opinion, especially at the subnational level.

Quick start
===========

Install from GitHub. `dgirt` requires [RStan](https://github.com/stan-dev/rstan). See the RStan [installation instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-install-rstan) for help.

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
#> Hierarchical parameters with modifiers:
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
#>                              n_eff      Rhat
#> theta_bar[MA__black,2006] 349.3893 0.9993043
#> theta_bar[NY__other,2006] 351.9231 1.0013305
#> theta_bar[MA__black,2006] 375.9871 0.9997203
#> theta_bar[NY__other,2006] 347.6678 1.0005613
#> theta_bar[MA__black,2006] 370.8096 0.9998836
#> theta_bar[NY__other,2006] 366.3508 1.0002863
#> theta_bar[NY__black,2007] 105.8143 1.0081298
#> theta_bar[MA__white,2007] 101.1465 1.0106646
#> theta_bar[NY__black,2007] 105.6818 1.0091991
#> theta_bar[MA__white,2007] 180.4446 1.0093761
#> theta_bar[NY__black,2007] 109.4179 1.0069398
#> theta_bar[MA__white,2007] 102.7480 1.0087389
#> theta_bar[MA__other,2008] 197.2584 1.0079659
#> theta_bar[NY__white,2008] 175.3147 1.0095019
#> theta_bar[MA__other,2008] 178.1905 1.0088156
#> theta_bar[NY__white,2008] 180.9532 1.0085106
#> theta_bar[MA__other,2008] 184.0831 1.0079557
#> theta_bar[NY__white,2008] 174.6215 1.0086977
```

Get posterior means with a convenience function:

``` r
get_posterior_mean(dgirt_out, pars = "theta_bar")
#>                           mean-chain:1 mean-chain:2 mean-all chains
#> theta_bar[MA__black,2006]    11.558036    11.429454       11.493745
#> theta_bar[NY__black,2007]    11.767744    11.314054       11.540899
#> theta_bar[MA__other,2008]    10.675021    10.487621       10.581321
#> theta_bar[NY__other,2006]    13.387433    13.008990       13.198212
#> theta_bar[MA__white,2007]     7.060649     6.977554        7.019101
#> theta_bar[NY__white,2008]     8.446796     8.287860        8.367328
#> theta_bar[MA__black,2006]    36.109720    37.227125       36.668422
#> theta_bar[NY__black,2007]    33.923667    34.262023       34.092845
#> theta_bar[MA__other,2008]    34.398405    35.328319       34.863362
#> theta_bar[NY__other,2006]    40.358194    40.874236       40.616215
#> theta_bar[MA__white,2007]    20.843050    22.372757       21.607903
#> theta_bar[NY__white,2008]    24.277230    25.152684       24.714957
#> theta_bar[MA__black,2006]   284.224807   237.578705      260.901756
#> theta_bar[NY__black,2007]   251.804622   218.204948      235.004785
#> theta_bar[MA__other,2008]   248.950215   217.664538      233.307377
#> theta_bar[NY__other,2006]   308.300503   271.317389      289.808946
#> theta_bar[MA__white,2007]   154.496565   147.377518      150.937042
#> theta_bar[NY__white,2008]   197.128599   175.250661      186.189630
```

Or generally access posterior samples:

``` r
apply(as.array(dgirt_out, pars = "theta_bar"), 3, mean)
#> theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
#>                 11.493745                 36.668422 
#> theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
#>                260.901756                 11.540899 
#> theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
#>                 34.092845                235.004785 
#> theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
#>                 10.581321                 34.863362 
#> theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
#>                233.307377                 13.198212 
#> theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
#>                 40.616215                289.808946 
#> theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
#>                  7.019101                 21.607903 
#> theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
#>                150.937042                  8.367328 
#> theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
#>                 24.714957                186.189630
```

See `help("dgirtfit-class")` and `help("stanfit-class")` for more.
