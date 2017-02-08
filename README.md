[![Build Status](https://travis-ci.org/jamesdunham/dgo.svg?branch=master)](https://travis-ci.org/jamesdunham/dgo) [![Build status](https://ci.appveyor.com/api/projects/status/1ta36kmoqen98k87?svg=true)](https://ci.appveyor.com/project/jamesdunham/dgo)

dgo is an R package for the dynamic estimation of group-level opinion. The package can be used to estimate subpopulation groups' average latent conservatism (or other latent trait) from individuals' responses to dichotomous questions using a Bayesian group-level IRT approach developed by [Caughey and Warshaw 2015](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html) that models latent traits at the level of demographic and/or geographic groups rather than individuals. This approach uses a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units, such as states.

dgo can also be used to estimate smoothed estimates of subpopulation groups' average responses on individual survey questions using a dynamic multi-level regression and poststratification (MRP) model ([Park, Gelman, and Bafumi 2004](http://stat.columbia.edu/~gelman/research/published/StateOpinionsNationalPolls.050712.dkp.pdf)). For instance, it could be used to estimate public opinion in each state on same-sex marriage or the Affordable Care Act.

This model opens up new areas of research on historical public opinion in the United States at the subnational level. It also enables scholars of comparative politics to estimate dynamic models of public opinion opinion at the country or subnational level.

Prerequisites
=============

Installation requires [RStan](http://mc-stan.org/interfaces/rstan.html) and its prerequisites, in particular a C++ toolchain. If you don't have RStan, follow its "[Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)" guide before continuing.

Installation
============

dgo can be installed from [GitHub](https://github.com/jamesdunham/dgo) using [devtools](https://github.com/hadley/devtools/):

``` r
devtools::install_github("jamesdunham/dgo", dependencies = TRUE)
```

Getting started
===============

``` r
library(dgo)
#> Loading required package: dgodata
#> Loading required package: Rcpp
#> Loading required package: rstan
#> Loading required package: ggplot2
#> Loading required package: StanHeaders
#> rstan (Version 2.14.1, packaged: 2016-12-28 14:55:41 UTC, GitRev: 5fa1e80eb817)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> rstan_options(auto_write = TRUE)
#> options(mc.cores = parallel::detectCores())
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

Abortion Attitudes
------------------

### Prepare input data with `shape`

DGIRT models are *dynamic*, so we need to specify which variable in the data represents time. They are also *group-level*, with groups defined by one variable for respondents' local geographic area and one or more variables for respondent characteristics.

The `time_filter` and `geo_filter` arguments optionally subset the data. Finally, `shape` requires the names of the survey identifier and survey weight variables in the data.

``` r
dgirt_in_abortion <- shape(opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "race3",
                  geo_filter = c("CA", "GA", "LA", "MA"),
                  id_vars = "source")
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
#> [1] "year"  "state" "race3"
#> Time periods:
#> [1] 2006 2007 2008 2009 2010
#> Local geographic areas:
#> [1] "CA" "GA" "LA" "MA"
#> Hierarchical parameters:
#> [1] "GA"         "LA"         "MA"         "race3other" "race3white"
#> Modifiers of hierarchical parameters:
#> NULL
#> Constants:
#>  Q  T  P  N  G  H  D 
#>  1  5  5 60 12  1  1
```

Response counts by state:

``` r
get_n(dgirt_in_abortion, by = c("state"))
#>    state     n
#> 1:    CA 14248
#> 2:    GA  4547
#> 3:    LA  1658
#> 4:    MA  2554
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

`dgirt` and `dgmrp` fit estimation models to data from `shape`. `dgirt` can be used to estimate a latent variable based on responses to multiple survey questions (e.g., latent policy conservatism), while `dgmrp` can be used to estimate public opinion on an individual survey question (e.g., abortion) using a dynamic multi-level regression and post-stratification (MRP) model. In this case, we use `dgmrp` to model abortion attitudes.

Under the hood, these functions use RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt` and `dgmrp`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out_abortion <- dgmrp(dgirt_in_abortion, iter = 1500, chains = 4, cores =
  4, seed = 42)
```

The model results are held in a `dgirtfit` object. Methods from RStan like `extract` are available if needed because `dgirtfit` is a subclass of `stanfit`. But dgo provides its own methods for typical post-estimation tasks.

### Work with `dgirt` or `dgmrp` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out_abortion)
#> dgirt samples from 4 chains of 1500 iterations, 750 warmup, thinned every 1 
#>   Drawn Wed Feb  8 12:24:24 2017 
#>   Package version 0.2.8 
#>   Model version 2017_01_04_singleissue 
#>   117 parameters; 60 theta_bars (year, state and race3)
#>   5 periods 2006 to 2010 
#> 
#> n_eff
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   15.80   89.68  264.50  483.90  597.70 3000.00
#> 
#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.9992  1.0050  1.0150  1.0250  1.0430  1.1090
#> 
#> Elapsed time
#>    chain warmup sample total
#> 1:     1    10S    20S   30S
#> 2:     2    10S    22S   32S
#> 3:     3    10S    19S   29S
#> 4:     4    13S    20S   33S
```

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
head(summarize(dgirt_out_abortion))
#>        param state race3 year      mean         sd    median     q_025
#> 1: theta_bar    CA black 2006 0.7754603 0.02155152 0.7760716 0.7316139
#> 2: theta_bar    CA black 2007 0.7988475 0.03076377 0.7971067 0.7442743
#> 3: theta_bar    CA black 2008 0.7225603 0.02428735 0.7222470 0.6753476
#> 4: theta_bar    CA black 2009 0.6846860 0.02226090 0.6847197 0.6395305
#> 5: theta_bar    CA black 2010 0.7394164 0.01584579 0.7399492 0.7078935
#> 6: theta_bar    CA other 2006 0.7319499 0.02315218 0.7322472 0.6878444
#>        q_975
#> 1: 0.8136820
#> 2: 0.8701612
#> 3: 0.7691617
#> 4: 0.7271336
#> 5: 0.7685217
#> 6: 0.7775199
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out_abortion, pars = "xi", funs = "var")
#>    param year        var
#> 1:    xi 2006 0.01946319
#> 2:    xi 2007 0.03932934
#> 3:    xi 2008 0.04135631
#> 4:    xi 2009 0.03546163
#> 5:    xi 2010 0.03086187
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
head(as.data.frame(dgirt_out_abortion))
#>        param state race3 year iteration     value
#> 1: theta_bar    CA black 2006         1 0.7614693
#> 2: theta_bar    CA black 2006         2 0.7505157
#> 3: theta_bar    CA black 2006         3 0.7915333
#> 4: theta_bar    CA black 2006         4 0.7742232
#> 5: theta_bar    CA black 2006         5 0.7778495
#> 6: theta_bar    CA black 2006         6 0.7457225
```

To poststratify the results use `poststratify`. The following example uses the group population proportions bundled as `annual_state_race_targets` to reweight and aggregate estimates to strata defined by state-years.

Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out_abortion, annual_state_race_targets, strata_names =
  c("state", "year"), aggregated_names = "race3")
#>     state year     value
#>  1:    CA 2006 0.7188081
#>  2:    CA 2007 0.7470397
#>  3:    CA 2008 0.6566884
#>  4:    CA 2009 0.6267420
#>  5:    CA 2010 0.6756431
#>  6:    GA 2006 0.6333206
#>  7:    GA 2007 0.6223014
#>  8:    GA 2008 0.5243312
#>  9:    GA 2009 0.5105648
#> 10:    GA 2010 0.5704081
#> 11:    LA 2006 0.5258743
#> 12:    LA 2007 0.4770990
#> 13:    LA 2008 0.4151797
#> 14:    LA 2009 0.4024384
#> 15:    LA 2010 0.4246879
#> 16:    MA 2006 0.7629225
#> 17:    MA 2007 0.8101844
#> 18:    MA 2008 0.7056092
#> 19:    MA 2009 0.6600767
#> 20:    MA 2010 0.7082504
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area. For this (unconverged) toy example we omit the CIs.

``` r
dgirt_plot(dgirt_out_abortion, y_min = NULL, y_max = NULL)
```

![](README/unnamed-chunk-14-1.png)

Output from `dgirt_plot` can be customized to some extent using objects from the ggplot2 package.

``` r
dgirt_plot(dgirt_out_abortion, y_min = NULL, y_max = NULL) + theme_classic()
```

![](README/unnamed-chunk-15-1.png)

`dgirt_plot` can also plot the `data.frame` output from `poststratify`. This requires arguments that identify the relevant variables in the `data.frame`. Below, `poststratify` aggregates over the demographic grouping variable `race3`, resulting in a `data.frame` of estimates by state-year. So, in the subsequent call to `dgirt_plot`, we pass the names of the state and year variables. The `group_names` argument is `NULL` because there are no grouping variables left after aggregating over `race3`.

``` r
ps <- poststratify(dgirt_out_abortion, annual_state_race_targets, strata_names =
  c("state", "year"), aggregated_names = "race3")
head(ps)
#>    state year     value
#> 1:    CA 2006 0.7188081
#> 2:    CA 2007 0.7470397
#> 3:    CA 2008 0.6566884
#> 4:    CA 2009 0.6267420
#> 5:    CA 2010 0.6756431
#> 6:    GA 2006 0.6333206
dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
```

![](README/unnamed-chunk-16-1.png)

Policy Liberalism
-----------------

### Prepare input data with `shape`

``` r
dgirt_in_liberalism <- shape(opinion, item_names = c("abortion",
    "affirmative_action","stemcell_research" , "gaymarriage_amendment",
    "partialbirth_abortion") , time_name = "year", geo_name = "state",
  group_names = "race3", geo_filter = c("CA", "GA", "LA", "MA"))
#> Applying restrictions, pass 1...
#>  Dropped 5 rows for missingness in covariates
#>  Dropped 8 rows for lacking item responses
#> Applying restrictions, pass 2...
#>  No changes
```

The reshaped and subsetted data can be summarized in a few ways before model fitting.

``` r
summary(dgirt_in_liberalism)
#> Items:
#> [1] "abortion"              "affirmative_action"    "gaymarriage_amendment"
#> [4] "partialbirth_abortion" "stemcell_research"    
#> Respondents:
#>    23,632 in `item_data`
#> Grouping variables:
#> [1] "year"  "state" "race3"
#> Time periods:
#> [1] 2006 2007 2008 2009 2010
#> Local geographic areas:
#> [1] "CA" "GA" "LA" "MA"
#> Hierarchical parameters:
#> [1] "GA"         "LA"         "MA"         "race3other" "race3white"
#> Modifiers of hierarchical parameters:
#> NULL
#> Constants:
#>   Q   T   P   N   G   H   D 
#>   5   5   5 300  12   1   1
```

Response counts by item-year:

``` r
get_item_n(dgirt_in_liberalism, by = "year")
#>    year abortion affirmative_action stemcell_research
#> 1: 2006     5275               4750              2483
#> 2: 2007     1690               1557              1705
#> 3: 2008     4697               4704              4002
#> 4: 2009     2141               2147                 0
#> 5: 2010     9204               9241              9146
#>    gaymarriage_amendment partialbirth_abortion
#> 1:                  2642                  5064
#> 2:                  1163                  1684
#> 3:                  4265                     0
#> 4:                     0                     0
#> 5:                  9226                     0
```

### Fit a model with `dgirt`

`dgirt` and `dgmrp` fit estimation models to data from `shape`. `dgirt` can be used to estimate a latent variable based on responses to multiple survey questions (e.g., latent policy conservatism), while `dgmrp` can be used to estimate public opinion on an individual survey question using a dynamic multi-level regression and post-stratification (MRP) model.

Under the hood, these functions use RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt` and `dgmrp`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out_liberalism <- dgirt(dgirt_in_liberalism, iter = 3000, chains = 4,
  cores = 4, seed = 42)
```

The model results are held in a `dgirtfit` object. Methods from RStan like `extract` are available if needed because `dgirtfit` is a subclass of `stanfit`. But dgo provides its own methods for typical post-estimation tasks.

### Work with `dgirt` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out_liberalism)
#> dgirt samples from 4 chains of 3000 iterations, 1500 warmup, thinned every 1 
#>   Drawn Wed Feb  8 12:29:09 2017 
#>   Package version 0.2.8 
#>   Model version 2017_01_04 
#>   137 parameters; 60 theta_bars (year, state and race3)
#>   5 periods 2006 to 2010 
#> 
#> n_eff
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   229.7   686.5  1026.0  1540.0  1563.0  6000.0
#> 
#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.9997  1.0000  1.0010  1.0020  1.0020  1.0150
#> 
#> Elapsed time
#>    chain warmup sample   total
#> 1:     1 1M 44S 1M 50S  2M 94S
#> 2:     2 1M 42S 2M 54S  3M 96S
#> 3:     3 1M 54S 1M 48S 2M 102S
#> 4:     4 1M 47S 1M 50S  2M 97S
```

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
head(summarize(dgirt_out_liberalism))
#>        param state race3 year      mean         sd    median      q_025
#> 1: theta_bar    CA black 2006 0.5181310 0.06903540 0.5155607 0.38979007
#> 2: theta_bar    CA black 2007 0.6132564 0.09604204 0.6052594 0.44996480
#> 3: theta_bar    CA black 2008 0.6091027 0.10859095 0.5952582 0.43748868
#> 4: theta_bar    CA black 2009 0.5204638 0.08900999 0.5085677 0.38176128
#> 5: theta_bar    CA black 2010 0.4992275 0.06126724 0.4930813 0.39782512
#> 6: theta_bar    CA other 2006 0.1508179 0.06059458 0.1573339 0.01016673
#>        q_975
#> 1: 0.6657002
#> 2: 0.8279937
#> 3: 0.8639421
#> 4: 0.7290341
#> 5: 0.6363504
#> 6: 0.2529650
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out_liberalism, pars = "xi", funs = "var")
#>    param year         var
#> 1:    xi 2006 0.011302809
#> 2:    xi 2007 0.007723304
#> 3:    xi 2008 0.006361194
#> 4:    xi 2009 0.005262130
#> 5:    xi 2010 0.005754282
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
head(as.data.frame(dgirt_out_liberalism))
#>        param state race3 year iteration     value
#> 1: theta_bar    CA black 2006         1 0.5326097
#> 2: theta_bar    CA black 2006         2 0.5249095
#> 3: theta_bar    CA black 2006         3 0.4810473
#> 4: theta_bar    CA black 2006         4 0.5842495
#> 5: theta_bar    CA black 2006         5 0.6794800
#> 6: theta_bar    CA black 2006         6 0.5321501
```

To poststratify the results use `poststratify`. The following example uses the group population proportions bundled as `annual_state_race_targets` to reweight and aggregate estimates to strata defined by state-years. Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out_liberalism, annual_state_race_targets, strata_names = c("state",
    "year"), aggregated_names = "race3")
#>     state year         value
#>  1:    CA 2006  0.1355272858
#>  2:    CA 2007  0.1848865835
#>  3:    CA 2008  0.1043107399
#>  4:    CA 2009  0.0459467646
#>  5:    CA 2010  0.0839898062
#>  6:    GA 2006  0.0964239685
#>  7:    GA 2007  0.0744225784
#>  8:    GA 2008 -0.0300159778
#>  9:    GA 2009 -0.0309101191
#> 10:    GA 2010  0.0003758105
#> 11:    LA 2006  0.0138033941
#> 12:    LA 2007 -0.0171435561
#> 13:    LA 2008 -0.1198432366
#> 14:    LA 2009 -0.1459680343
#> 15:    LA 2010 -0.1016888955
#> 16:    MA 2006  0.1398600252
#> 17:    MA 2007  0.2725613449
#> 18:    MA 2008  0.1556987370
#> 19:    MA 2009  0.0713680458
#> 20:    MA 2010  0.1154100785
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area. For this (unconverged) toy example we omit the CIs.

``` r
dgirt_plot(dgirt_out_liberalism, y_min = NULL, y_max = NULL)
```

![](README/unnamed-chunk-26-1.png)

`dgirt_plot` can also plot the `data.frame` output from `poststratify`. This requires arguments that identify the relevant variables in the `data.frame`. Below, `poststratify` aggregates over the demographic grouping variable `race3`, resulting in a `data.frame` of estimates by state-year. So, in the subsequent call to `dgirt_plot`, we pass the names of the state and year variables. The `group_names` argument is `NULL` because there are no grouping variables left after aggregating over `race3`.

``` r
ps <- poststratify(dgirt_out_liberalism, annual_state_race_targets, strata_names = c("state",
    "year"), aggregated_names = "race3")
head(ps)
#>    state year      value
#> 1:    CA 2006 0.13552729
#> 2:    CA 2007 0.18488658
#> 3:    CA 2008 0.10431074
#> 4:    CA 2009 0.04594676
#> 5:    CA 2010 0.08398981
#> 6:    GA 2006 0.09642397
dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
```

![](README/unnamed-chunk-27-1.png)

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

> Dunham, James, Devin Caughey, and Christopher Warshaw. 2017. dgo: Dynamic Estimation of Group-level Opinion. R package. <https://jamesdunham.github.io/dgo/>.
