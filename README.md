[![Build Status](https://travis-ci.org/jamesdunham/dgo.svg?branch=master)](https://travis-ci.org/jamesdunham/dgo)

dgo is an R package for the dynamic estimation of group-level opinion. The package can be used to estimate subpopulation groups' average latent conservatism (or other latent trait) from individuals' responses to dichotomous questions using a Bayesian group-level IRT approach developed by [Caughey and Warshaw 2015](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html) that models latent traits at the level of demographic and/or geographic groups rather than individuals. This approach uses a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units, such as states. 

dgo can also be used to estimate smoothed estimates of subpopulation groups' average responses on individual survey questions using a dynamic multi-level regression and poststratification (MRP) model ([Park, Gelman, and Bafumi  2004](http://stat.columbia.edu/~gelman/research/published/StateOpinionsNationalPolls.050712.dkp.pdf)). For instance, it could be used to estimate public opinion in each state on same-sex marriage or the Affordable Care Act.

This model opens up new areas of research on historical public opinion in the United States at the subnational level. It also enables scholars of comparative politics to estimate dynamic models of public opinion opinion at the country or subnational level.

[This document](https://github.com/jamesdunham/dgo/blob/master/inst/dgirt_details.pdf) describes the model in detail. 

Prerequisites
-------------

Installation requires [RStan](http://mc-stan.org/interfaces/rstan.html) and its prerequisites, in particular a C++ toolchain. If you don't have RStan, follow its "[Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)" guide before continuing.

Installation
------------

dgo can be installed from [GitHub](https://github.com/jamesdunham/dgo) using [devtools](https://github.com/hadley/devtools/):

    devtools::install_github("jamesdunham/dgo", dependencies = TRUE)

Getting started
---------------

``` r
library(dgo)
#> Loading required package: Rcpp
```

The minimal workflow from raw data to estimation is:

1.  shape input data using the `shape` function; and
2.  pass the result to the `dgirt` function to estimate a latent trait (e.g., conservatism) or `dgmrp` function to estimate opinion on a single survey question.

### Set RStan options

These are RStan's recommended options on a local, multicore machine with excess RAM:

``` r
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

We show two illustrative applications. First, we show an application for estimating public opinion on abortion. Second, we show an application for estimating latent policy liberalism.  

## Abortion Attitudes

### Prepare input data with `shape`

DGIRT models are *dynamic*, so we need to specify which variable in the data represents time. They are also *group-level*, with groups defined by one variable for respondents' local geographic area and one or more variables for respondent characteristics.

The `time_filter` and `geo_filter` arguments optionally subset the data. Finally, `shape` requires the names of the survey identifier and survey weight variables in the data.

``` r
dgirt_in_abortion <- shape(opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "race",
                  geo_filter = c("CA", "GA", "LA", "MA"),
                  survey_name = "source",
                  weight_name = "weight")
#> Applying restrictions, pass 1...
#>  Dropped 5 rows for missingness in covariates
#>  Dropped 633 rows for lacking item responses
#> Applying restrictions, pass 2...
#>  No changes
```

The reshaped and subsetted data can be summarized in a few ways before model fitting.

``` r
summary(dgirt_in_abortion)
#> Items:
#> [1] "abortion"
#> Respondents:
#>    23,007 in `item_data`
#> Grouping variables:
#> [1] "year"  "state" "race" 
#> Time periods:
#> [1] 2006 2007 2008 2009 2010
#> Local geographic areas:
#> [1] "CA" "GA" "LA" "MA"
#> Hierarchical parameters:
#> [1] "GA"        "LA"        "MA"        "raceother" "racewhite"
#> Modifiers of hierarchical parameters:
#> character(0)
#> Constants:
#>  Q  T  P  N  G  H  D 
#>  1  5  5 60 12  1  1 
 ```

Response counts by survey-year:

``` r
get_n(dgirt_in_abortion, by = c("year", "source"))
#>    year    source    n
#> 1: 2006 CCES_2006 5275
#> 2: 2007 CCES_2007 1690
#> 3: 2008 CCES_2008 4697
#> 4: 2009 CCES_2009 2141
#> 5: 2010 CCES_2010 9204
```

Response counts by item-year:

``` r
get_item_n(dgirt_in_abortion, by = "year")
#>    year abortion
#> 1: 2006     5275
#> 2: 2007     1690
#> 3: 2008     4697
#> 4: 2009     2141
#> 5: 2010     9204

```

### Fit a model with `dgirt` or `dgmrp`

`dgirt` and `dgmrp` fit estimation models to data from `shape`. `dgirt` can be used to estimate a latent variable based on responses to multiple survey questions (e.g., latent policy conservatism), while `dgmrp` can be used to estimate public opinion on an individual survey question using a dynamic multi-level regression and post-stratification (MRP) model.  

Under the hood, these functions use RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt` and `dgmrp`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out_abortion <- dgmrp(dgirt_in_abortion, iter = 1500, chains = 4, cores = 4, seed = 42,
                   refresh = 500)
```

The model results are held in a `dgirtfit` object. Methods from RStan like `extract` are available if needed because `dgirtfit` is a subclass of `stanfit`. But dgo provides its own methods for typical post-estimation tasks.

### Work with `dgirt` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out_abortion)
#> dgirt samples from 4 chains of 1500 iterations, 750 warmup, thinned every 1 
#>   Drawn Fri Jan  6 23:35:42 2017 
#>   Package version 0.2.7 
#>   Model version 2017_01_04_singleissue 
#>   117 parameters; 60 theta_bars (year, state and race)
#>   5 periods 2006 to 2010 

#> n_eff
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    9.279  317.400 1038.000 1284.000 2111.000 3000.000 

#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.9992  1.0010  1.0030  1.0140  1.0150  1.6230 

#> Elapsed time
#>    chain warmup sample total
#> 1:     1    16S    10S   26S
#> 2:     2    18S    20S   38S
#> 3:     3    18S    19S   37S
#> 4:     4    15S    20S   35S


To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
head(summarize(dgirt_out_abortion))
#>        param state  race year      mean         sd    median     q_025     q_975
#> 1: theta_bar    CA black 2006 0.7173553 0.07662729 0.7174894 0.5662267 0.8702876
#> 2: theta_bar    CA black 2007 0.7508767 0.10914309 0.7418550 0.5545848 0.9806262
#> 3: theta_bar    CA black 2008 0.4787463 0.07473553 0.4773249 0.3360598 0.6365967
#> 4: theta_bar    CA black 2009 0.3994269 0.07008417 0.3993055 0.2611861 0.5383515
#> 5: theta_bar    CA black 2010 0.5222371 0.07595806 0.5241843 0.3691531 0.6693409
#> 6: theta_bar    CA other 2006 0.5642023 0.07408100 0.5652054 0.4232060 0.7116074
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out_abortion, pars = "xi", funs = "var")
#>   param year        var
#> 1:    xi 2006 0.02247365
#> 2:    xi 2007 0.03122596
#> 3:    xi 2008 0.02850483
#> 4:    xi 2009 0.02429801
#> 5:    xi 2010 0.02585458
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
head(as.data.frame(dgirt_out_abortion))
#>        param state  race year iteration     value
#> 1: theta_bar    CA black 2006         1 0.7486239
#> 2: theta_bar    CA black 2006         2 0.6086190
#> 3: theta_bar    CA black 2006         3 0.5896640
#> 4: theta_bar    CA black 2006         4 0.7733479
#> 5: theta_bar    CA black 2006         5 0.7697022
#> 6: theta_bar    CA black 2006         6 0.7419776
```

To poststratify the results use `poststratify`. The following example uses the group population proportions bundled as `state_year_targets` to reweight and aggregate estimates to strata defined by state-years. You can convert the probit scale to the response scale using the "single_issue" parameter.  Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out_abortion, state_year_targets, strata_names = c("state", "year"),
             aggregated_names = "race", single_issue="T")
#>     state year       value
#> 1:    CA 2006  0.55943278
#> 2:    CA 2007  0.58452441
#> 3:    CA 2008  0.36074873
#> 4:    GA 2006  0.31574732
#> 5:    GA 2007  0.19140911
#> 6:    GA 2008  0.03079019
#> 7:    LA 2006 -0.04283603
#> 8:    LA 2007 -0.22322708
#> 9:    LA 2008 -0.22612453
#> 10:    MA 2006  0.70998185
#> 11:    MA 2007  0.92555553
#> 12:    MA 2008  0.52063614
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area. For this (unconverged) toy example we omit the CIs.

``` r
dgirt_plot(dgirt_out_abortion, y_min = NULL, y_max = NULL, single_issue="T")
```

![](README-unnamed-chunk-13-2.png)

Output from `dgirt_plot` can be customized to some extent using objects from the ggplot2 package.

``` r
dgirt_plot(dgirt_out_abortion, y_min = NULL, y_max = NULL, single_issue="T") + theme_classic()
```

![](README-unnamed-chunk-14-2.png)

`dgirt_plot` can also plot the `data.frame` output from `poststratify`. This requires arguments that identify the relevant variables in the `data.frame`. Below, `poststratify` aggregates over the demographic grouping variable `race`, resulting in a `data.frame` of estimates by state-year. So, in the subsequent call to `dgirt_plot`, we pass the names of the state and year variables. The `group_names` argument is `NULL` because there are no grouping variables left after aggregating over `race`.

``` r
ps <- poststratify(dgirt_out_abortion, state_year_targets,
                   strata_names = c("state", "year"), aggregated_names = "race", single_issue="T")
head(ps)
#>    state year      value
#> 1:    CA 2006 0.7116304
#> 2:    CA 2007 0.7197962
#> 3:    CA 2008 0.6405804
#> 4:    GA 2006 0.6206845
#> 5:    GA 2007 0.5738724
#> 6:    GA 2008 0.5120130

dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
```

![](README-unnamed-chunk-15-2.png)


## Policy Liberalism

### Prepare input data with `shape`

``` r
dgirt_in_liberalism <- shape(opinion,
                  item_names = c("abortion", "affirmative_action","stemcell_research" , "gaymarriage_amendment",
                  "partialbirth_abortion") ,
                  time_name = "year",
                  geo_name = "state",
                  group_names = "race",
                  geo_filter = c("CA", "GA", "LA", "MA"),
                  survey_name = "source",
                  weight_name = "weight")
#> Applying restrictions, pass 1...
#> 	Dropped 5 rows for missingness in covariates
#> 	Dropped 8 rows for lacking item responses
#> Applying restrictions, pass 2...
#> 	No changes
```

The reshaped and subsetted data can be summarized in a few ways before model fitting.

``` r
summary(dgirt_in_liberalism)
#> 	Items:
#> 	[1] "abortion"              "affirmative_action"    "gaymarriage_amendment" #> 	"partialbirth_abortion" "stemcell_research"    
#> 	Respondents:
#> 	   23,632 in `item_data`
#> 	Grouping variables:
#> 	[1] "year"  "state" "race" 
#> 	Time periods:
#> 	[1] 2006 2007 2008 2009 2010
#> 	Local geographic areas:
#> 	[1] "CA" "GA" "LA" "MA"
#> 	Hierarchical parameters:
#> 	[1] "GA"        "LA"        "MA"        "raceother" "racewhite"
#> 	Modifiers of hierarchical parameters:
#> 	character(0)
#> 	Constants:
#> 	  Q   T   P   N   G   H   D 
#> 	  5   5   5 300  12   1   1  ```

Response counts by item-year:

``` r
#> 	> get_item_n(dgirt_in_liberalism, by = "year")
#> 	   year abortion affirmative_action stemcell_research gaymarriage_amendment 	partialbirth_abortion
#> 	1: 2006     5275               4750              2483                  2642                   	5064
#> 	2: 2007     1690               1557              1705                  1163                   	1684
#> 	3: 2008     4697               4704              4002                  4265                      	0
#> 	4: 2009     2141               2147                 0                     0                      	0
#> 	5: 2010     9204               9241              9146                  9226                      	0


```

### Fit a model with `dgirt`

`dgirt` and `dgmrp` fit estimation models to data from `shape`. `dgirt` can be used to estimate a latent variable based on responses to multiple survey questions (e.g., latent policy conservatism), while `dgmrp` can be used to estimate public opinion on an individual survey question using a dynamic multi-level regression and post-stratification (MRP) model.  

Under the hood, these functions use RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt` and `dgmrp`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out_liberalism <- dgirt(dgirt_in_liberalism, iter = 1500, chains = 4, cores = 4, seed = 42,
                   refresh = 500)
```

The model results are held in a `dgirtfit` object. Methods from RStan like `extract` are available if needed because `dgirtfit` is a subclass of `stanfit`. But dgo provides its own methods for typical post-estimation tasks.

### Work with `dgirt` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out_liberalism)
#> > summary(dgirt_out_liberalism)
#> dgirt samples from 4 chains of 1500 iterations, 750 warmup, thinned every 1 
#>   Drawn Mon Jan  9 06:58:24 2017 
#>   Package version 0.2.7 
#>   Model version 2017_01_04 
#>   137 parameters; 60 theta_bars (year, state and race)
#>   5 periods 2006 to 2010 

#> n_eff
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   53.53  134.30  237.40  358.90  373.30 1999.00 

#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.000   1.007   1.013   1.017   1.022   1.064 

#> Elapsed time
#>    chain warmup sample  total
#> 1:     1 1M 29S 2M 41S 3M 70S
#> 2:     2 1M 41S 2M 33S 3M 74S
#> 3:     3  1M 6S 2M 34S 3M 40S
#> 4:     4 1M 55S 2M 30S 3M 85S

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
head(summarize(dgirt_out_liberalism))
#>        param state  race year      mean         sd    median     q_025     q_975
#> 1: theta_bar    CA black 2006 0.5495122 0.08408488 0.5417941  0.40123321 0.7383885
#> 2: theta_bar    CA black 2007 0.5836415 0.10280742 0.5729066  0.41597341 0.8161483
#> 3: theta_bar    CA black 2008 0.5591099 0.10671551 0.5459816  0.38828767 0.8002534
#> 4: theta_bar    CA black 2009 0.4900812 0.09874492 0.4727758  0.34593141 0.7411483
#> 5: theta_bar    CA black 2010 0.4515506 0.07719338 0.4440954  0.32062133 0.6175475
#> 6: theta_bar    CA other 2006 0.1539503 0.06932572 0.1610716 -0.01282908 0.2726472
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out_liberalism, pars = "xi", funs = "var")
#>   param year        var
#> 1:    xi 2006 0.021263542
#> 2:    xi 2007 0.008842126
#> 3:    xi 2008 0.008028949
#> 4:    xi 2009 0.007556206
#> 5:    xi 2010 0.007754970
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
head(as.data.frame(dgirt_out_liberalism))
#>        param state  race year iteration     value
#> 1: theta_bar    CA black 2006         1 0.5022184
#> 2: theta_bar    CA black 2006         2 0.4988011
#> 3: theta_bar    CA black 2006         3 0.3952569
#> 4: theta_bar    CA black 2006         4 0.4325513
#> 5: theta_bar    CA black 2006         5 0.5027754
#> 6: theta_bar    CA black 2006         6 0.5107315
```

To poststratify the results use `poststratify`. The following example uses the group population proportions bundled as `state_year_targets` to reweight and aggregate estimates to strata defined by state-years. Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out_liberalism, state_year_targets, strata_names = c("state", "year"),
             aggregated_names = "race")
#>     state year       value
 1:    CA 2006  0.156577644
 2:    CA 2007  0.169483043
 3:    CA 2008  0.133370493
 4:    GA 2006  0.078822376
 5:    GA 2007  0.006179967
 6:    GA 2008 -0.041920152
 7:    LA 2006 -0.028274537
 8:    LA 2007 -0.088095393
 9:    LA 2008 -0.118881988
10:    MA 2006  0.152667148
11:    MA 2007  0.230784237
12:    MA 2008  0.160877693

```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area. For this (unconverged) toy example we omit the CIs.

``` r
dgirt_plot(dgirt_out_liberalism, y_min = NULL, y_max = NULL)
```

![](README-unnamed-chunk-13-3.png)

`dgirt_plot` can also plot the `data.frame` output from `poststratify`. This requires arguments that identify the relevant variables in the `data.frame`. Below, `poststratify` aggregates over the demographic grouping variable `race`, resulting in a `data.frame` of estimates by state-year. So, in the subsequent call to `dgirt_plot`, we pass the names of the state and year variables. The `group_names` argument is `NULL` because there are no grouping variables left after aggregating over `race`.

``` r
ps <- poststratify(dgirt_out_liberalism, state_year_targets,
                   strata_names = c("state", "year"), aggregated_names = "race")
head(ps)
#>    state year      value
#> 1:    CA 2006 0.7116304
#> 2:    CA 2007 0.7197962
#> 3:    CA 2008 0.6405804
#> 4:    GA 2006 0.6206845
#> 5:    GA 2007 0.5738724
#> 6:    GA 2008 0.5120130

dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
```

![](README-unnamed-chunk-15-3.png)

Troubleshooting
---------------

Please [report issues](https://github.com/jamesdunham/dgo/issues) that you encounter.

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

dgo is under development and we welcome [suggestions](https://github.com/jamesdunham/dgo/issues).

The package citation is

> Dunham, James, Devin Caughey, and Christopher Warshaw. 2016. dgo: Dynamic Estimation of Group-level Opinion. R package. <https://jamesdunham.github.io/dgo/>.
